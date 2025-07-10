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

import java.io.IOException;
import java.util.*;

import jakarta.inject.Inject;

import com.folib.configuration.ConfigurationManager;
import org.apache.commons.collections4.CollectionUtils;
import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author veadan
 */
@Component
public class LocalStorageProxyRepositoryExpiredArtifactsCleaner
{

    private final Logger logger = LoggerFactory.getLogger(LocalStorageProxyRepositoryExpiredArtifactsCleaner.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactRepository artifactEntityRepository;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Transactional
    public void cleanup(final Integer lastAccessedTimeInDays,
                        final Long minSizeInBytes)
            throws IOException
    {
        final Page<Artifact> artifactEntries = artifactEntityRepository.findMatching(lastAccessedTimeInDays, minSizeInBytes,
                                                                                     PageRequest.of(0, 1));
        List<Artifact> artifactsToDelete = filterAccessibleProxiedArtifacts(artifactEntries.toList());
        if (artifactsToDelete.isEmpty())
        {
            return;
        }

        logger.info("Cleaning artifacts {}", artifactsToDelete.size());
        deleteFromStorage(artifactsToDelete);
    }

    private List<Artifact> filterAccessibleProxiedArtifacts(final List<Artifact> artifactEntries)
    {
        if (CollectionUtils.isEmpty(artifactEntries))
        {
            return Collections.emptyList();
        }
        
        List<Artifact> result = new ArrayList<>();
        for (final Iterator<Artifact> it = artifactEntries.iterator(); it.hasNext(); )
        {
            final Artifact artifactEntry = it.next();
            final Storage storage = configurationManager.getConfiguration().getStorage(artifactEntry.getStorageId());
            if (Objects.isNull(storage)) {
                result.add(artifactEntry);
                continue;
            }
            final Repository repository = storage.getRepository(artifactEntry.getRepositoryId());
            if (Objects.isNull(repository)) {
                result.add(artifactEntry);
                continue;
            }
            if (!repository.isProxyRepository())
            {
                continue;
            }
            final RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null)
            {
                logger.warn("Repository {} is not associated with remote repository", repository.getId());
                continue;
            }
            if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository))
            {
                logger.warn("Remote repository {} is down. Artifacts won't be cleaned up.", remoteRepository.getUrl());
                continue;
            }
            
            result.add(artifactEntry);
        }
        
        return result;
    }

    private void deleteFromStorage(final List<Artifact> artifactEntries)
            throws IOException
    {
        for (final Artifact artifactEntry : artifactEntries)
        {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getArtifactPath());
            artifactManagementService.delete(repositoryPath, true);
        }
    }

}
