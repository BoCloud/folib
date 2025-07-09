/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.providers.repository;

import com.folib.components.DistributedCacheComponent;
import com.folib.data.criteria.Paginator;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.providers.io.*;
import com.folib.io.RepositoryStreamReadContext;
import com.folib.io.RepositoryStreamWriteContext;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactManagementService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

/**
 * @author Veadan
 * @author veadan
 */
@Component
public class ProxyRepositoryProvider
        extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryProvider.class);

    private static final String ALIAS = "proxy";

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private HostedRepositoryProvider hostedRepositoryProvider;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath path)
            throws IOException {
        return hostedRepositoryProvider.getInputStreamInternal(path);
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath targetPath = hostedRepositoryProvider.fetchPath(repositoryPath);
        if (targetPath == null) {
            targetPath = resolvePathExclusive(repositoryPath);
        } else if (RepositoryFiles.hasRefreshContent(targetPath)) {
            targetPath = resolvePathExclusive(repositoryPath);
            if (Objects.isNull(targetPath)) {
                targetPath = hostedRepositoryProvider.fetchPath(repositoryPath);
            }
        }
        if (Objects.nonNull(targetPath) && RepositoryFiles.hasExpired(targetPath) && !Files.isDirectory(targetPath)) {
            if (StringUtils.isNotBlank(repositoryPath.getArtifactPath())) {
                eventPublisher.publishEvent(new ProxyRepositoryPathExpiredEvent(repositoryPathResolver.resolve(targetPath.getRepository(), repositoryPath.getArtifactPath())));
            } else {
                eventPublisher.publishEvent(new ProxyRepositoryPathExpiredEvent(targetPath));
            }
        }
        return targetPath;
    }

    public RepositoryPath resolvePathExclusive(RepositoryPath repositoryPath)
            throws IOException {
        try {
            if (Boolean.TRUE.equals(repositoryPath.getDisableRemote())) {
                return null;
            }
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryPath.getRepository().getLayout());
            layoutProvider.targetUrl(repositoryPath);
            return proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
        } catch (IOException e) {
            logger.error("Failed to resolve Path for proxied artifact [{}]",
                    repositoryPath, e);

            throw e;
        }
    }

    @Override
    protected OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        if (Objects.isNull(predicate.getNotPublishEvent()) || Boolean.FALSE.equals(predicate.getNotPublishEvent())) {
            RemoteRepositorySearchEvent event = new RemoteRepositorySearchEvent(storageId,
                    repositoryId,
                    predicate,
                    paginator);
            eventPublisher.publishEvent(event);
        }

        return hostedRepositoryProvider.search(storageId, repositoryId, predicate, paginator);
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        return hostedRepositoryProvider.count(storageId, repositoryId, predicate);
    }

    @Override
    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException {
        Artifact artifactEntry = super.provideArtifact(repositoryPath);
        if (artifactEntry.getNativeId() == null) {
            artifactEntry = new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                    RepositoryFiles.readCoordinates(repositoryPath));
            artifactEntry.setArtifactFileExists(Boolean.FALSE);
        }

        return artifactEntry;
    }

    @Override
    protected boolean shouldStoreArtifact(Artifact artifactEntry) {
        boolean result = super.shouldStoreArtifact(artifactEntry) || !artifactEntry.getArtifactFileExists();
        artifactEntry.setArtifactFileExists(true);

        return result;
    }

    @Override
    public void commit(RepositoryStreamWriteContext ctx)
            throws IOException {
        super.commit(ctx);
    }

    @Override
    public void commitStoreIndex(RepositoryStreamReadContext ctx)
            throws IOException {
        super.commitStoreIndex(ctx);
    }

    @Override
    public void onStoreIndexAfter(RepositoryStreamReadContext ctx)
            throws IOException {
        super.onStoreIndexAfter(ctx);
    }

}
