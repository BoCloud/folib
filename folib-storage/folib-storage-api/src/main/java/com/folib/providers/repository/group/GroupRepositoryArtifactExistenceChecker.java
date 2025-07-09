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
package com.folib.providers.repository.group;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.inject.Inject;

import com.folib.configuration.ConfigurationManager;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.mutable.MutableBoolean;
import com.folib.configuration.ConfigurationUtils;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class GroupRepositoryArtifactExistenceChecker
{

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    public boolean artifactExistsInTheGroupRepositorySubTree(final Repository groupRepository,
                                                             final RepositoryPath repositoryPath)
            throws IOException
    {
        return artifactExistsInTheGroupRepositorySubTree(groupRepository, repositoryPath, new HashMap<>());
    }

    public boolean artifactExistsInTheGroupRepositorySubTree(final Repository groupRepository,
                                                             final RepositoryPath repositoryPath,
                                                             final Map<String, MutableBoolean> repositoryArtifactExistence)
            throws IOException
    {
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(groupRepository, storageAndRepositoryIdList);
        for (final String maybeStorageAndRepositoryId : storageAndRepositoryIdList)
        {
            final String subStorageId = getStorageId(groupRepository, maybeStorageAndRepositoryId);
            final String subRepositoryId = getRepositoryId(maybeStorageAndRepositoryId);
            final Repository subRepository = getRepository(subStorageId, subRepositoryId);

            final String storageAndRepositoryId = subStorageId + ":" + subRepositoryId;
            repositoryArtifactExistence.putIfAbsent(storageAndRepositoryId, new MutableBoolean());
            if (repositoryArtifactExistence.get(storageAndRepositoryId).isTrue())
            {
                return true;
            }

            if (subRepository.isGroupRepository())
            {
                boolean artifactExistence = artifactExistsInTheGroupRepositorySubTree(subRepository,
                                                                                      repositoryPath,
                                                                                      repositoryArtifactExistence);
                if (artifactExistence)
                {
                    repositoryArtifactExistence.get(storageAndRepositoryId).setTrue();
                    return true;
                }
            }
            else
            {
                final LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(subRepository.getLayout());
                RepositoryPath subRepositoryPath = repositoryPathResolver.resolve(subRepository, repositoryPath.relativize());
                
                if (RepositoryFiles.artifactExists(subRepositoryPath))
                {
                    repositoryArtifactExistence.get(storageAndRepositoryId).setTrue();
                    return true;
                }
            }
        }
        return false;
    }

    private Repository getRepository(final String subStorageId,
                                     final String subRepositoryId)
    {
        return configurationManager.getConfiguration()
                                   .getStorage(subStorageId)
                                   .getRepository(subRepositoryId);
    }

    private String getRepositoryId(final String maybeStorageAndRepositoryId)
    {
        return ConfigurationUtils.getRepositoryId(maybeStorageAndRepositoryId);
    }

    private String getStorageId(final Repository groupRepository,
                                final String maybeStorageAndRepositoryId)
    {
        return ConfigurationUtils.getStorageId(groupRepository.getStorage().getId(),
                                               maybeStorageAndRepositoryId);
    }

}
