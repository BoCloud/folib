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
package com.folib.providers.repository.proxied;

import com.folib.client.RestArtifactResolver;
import com.folib.config.HelmRepoUtil;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathLock;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.function.Function;

/**
 * @author veadan
 */
@Component
public class ProxyRepositoryArtifactResolver {
    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryArtifactResolver.class);

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RestArtifactResolverFactory restArtifactResolverFactory;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    private HelmRepoUtil helmRepoUtil;
    @Inject
    private List<FallbackRemoteArtifactInputStreamFactory> fallbackRemoteArtifactInputStreamRegistry;

    /**
     * This method has been developed to force fetch resource from remote.
     * <p>
     * It should not contain any local / cache existence checks.
     * <p>
     * Update this method carefully.
     */
    public RepositoryPath fetchRemoteResource(RepositoryPath repositoryPath)
            throws IOException {
        Repository repository = repositoryPath.getFileSystem().getRepository();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            logger.warn("Remote repository '{}' is down.", remoteRepository.getUrl());
            return null;
        }

        RestArtifactResolver client = restArtifactResolverFactory.newInstance(remoteRepository,repositoryPath);
        Function<Exception, InputStream> fallback = null;
        for (FallbackRemoteArtifactInputStreamFactory fallbackRemoteArtifactInputStreamFactory : fallbackRemoteArtifactInputStreamRegistry) {
            if (repositoryPath.getRepository().getLayout().equals(fallbackRemoteArtifactInputStreamFactory.getLayout())){
                fallback = fallbackRemoteArtifactInputStreamFactory.getFallbackRemoteArtifactInputStream(repositoryPath);
                break;
            }
        }
        InputStream inputStream = new ProxyRepositoryInputStream(client, repositoryPath);
        if (fallback != null) {
            inputStream = new FallbackRemoteArtifactInputStream(inputStream, fallback);
        }

        try (InputStream is = new BufferedInputStream(inputStream)) {
            return doFetch(repositoryPath, is);
        }
    }

    private RepositoryPath doFetch(RepositoryPath repositoryPath,
                                   InputStream is)
            throws IOException {
        //We need this to force initialize lazy connection to remote repository.
        int available = is.available();
        logger.info("Got [{}] available bytes for [{}].", available, repositoryPath);

        RepositoryPath result = onSuccessfulProxyRepositoryResponse(is, repositoryPath);
        if (RepositoryFiles.isArtifact(repositoryPath)) {
            artifactEventListenerRegistry.dispatchArtifactFetchedFromRemoteEvent(result);
        }
        return result;
    }

    protected RepositoryPath onSuccessfulProxyRepositoryResponse(InputStream is,
                                                                 RepositoryPath repositoryPath)
            throws IOException {
        artifactManagementService.store(repositoryPath, is);
        // helm 代理修改索引
        boolean indexFlag = repositoryPath.getRepository().getLayout().equalsIgnoreCase("helm")
                && repositoryPath.toString().endsWith("index.yaml");
        if (indexFlag) {
            helmRepoUtil.reloadIndex(repositoryPath);
            logger.info("Reload helm index");
        }
        // TODO: Add a policy for validating the checksums of downloaded artifacts
        // TODO: Validate the local checksum against the remote's checksums    徐新平
        // Serve the downloaded artifact
        return repositoryPath;
    }

}
