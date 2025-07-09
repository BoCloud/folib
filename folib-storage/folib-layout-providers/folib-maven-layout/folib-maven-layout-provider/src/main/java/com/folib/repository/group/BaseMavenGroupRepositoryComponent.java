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
package com.folib.repository.group;

import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.repository.group.GroupRepositoryArtifactExistenceChecker;
import com.folib.providers.repository.group.GroupRepositorySetCollector;
import com.folib.services.support.ArtifactRoutingRulesChecker;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public abstract class BaseMavenGroupRepositoryComponent
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    protected LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    protected GroupRepositorySetCollector groupRepositorySetCollector;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private GroupRepositoryArtifactExistenceChecker groupRepositoryArtifactExistenceChecker;

    @Inject
    private ArtifactRoutingRulesChecker artifactRoutingRulesChecker;
    
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    public void cleanupGroupsContaining(RepositoryPath repositoryPath)
            throws IOException
    {
        cleanupGroupsContaining(repositoryPath, new HashMap<>());
    }

    private void cleanupGroupsContaining(RepositoryPath repositoryPath,
                                         final Map<String, MutableBoolean> repositoryArtifactExistence)
            throws IOException
    {
        Repository repository = repositoryPath.getRepository();
        final List<Repository> directParents = configurationManagementService.getConfiguration()
                                                                             .getGroupRepositoriesContaining(repository.getStorage().getId(),
                                                                                                                      repository.getId());
        if (CollectionUtils.isEmpty(directParents))
        {
            return;
        }
        
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        
        for (final Repository groupRepository : directParents)
        {

            boolean artifactExists = groupRepositoryArtifactExistenceChecker.artifactExistsInTheGroupRepositorySubTree(groupRepository,
                                                                                                                       repositoryPath,
                                                                                                                       repositoryArtifactExistence);

            if (!artifactExists)
            {
                cleanupGroupWhenArtifactPathNoLongerExistsInSubTree(groupRepository, artifactPath);
            }
            
            cleanupGroupsContaining(repositoryPathResolver.resolve(groupRepository, repositoryPath),
                                    repositoryArtifactExistence);
        }
    }

    protected abstract void cleanupGroupWhenArtifactPathNoLongerExistsInSubTree(Repository groupRepository,
                                                                                String artifactPath)
            throws IOException;


    public void updateGroupsContaining(RepositoryPath repositoryPath)
            throws IOException
    {

        final UpdateCallback updateCallback = newInstance(repositoryPath);
        try
        {
            updateCallback.beforeUpdate();
        }
        catch (StopUpdateSilentlyException ex)
        {
            return;
        }


        Repository repository = repositoryPath.getRepository();
        updateGroupsContaining(repositoryPath, Lists.newArrayList(repository), updateCallback);
    }

    private void updateGroupsContaining(final RepositoryPath repositoryPath,
                                        final List<Repository> leafRoute,
                                        final UpdateCallback updateCallback)
            throws IOException
    {
        Repository repository = repositoryPath.getRepository();
        final List<Repository> groupRepositories = configurationManagementService.getConfiguration()
                                                                                 .getGroupRepositoriesContaining(repository.getStorage().getId(),
                                                                                                                 repository.getId());
        if (CollectionUtils.isEmpty(groupRepositories))
        {
            return;
        }
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        for (final Repository parent : groupRepositories)
        {
            RepositoryPath parentRepositoryArtifactAbsolutePath = repositoryPathResolver.resolve(parent, repositoryPath);
            
            if (!isOperationDeniedByRoutingRules(parent, leafRoute, artifactPath))
            {
                updateCallback.performUpdate(parentRepositoryArtifactAbsolutePath);
            }

            leafRoute.add(parent);

            updateGroupsContaining(parentRepositoryArtifactAbsolutePath, leafRoute, updateCallback);

            leafRoute.remove(parent);
        }
    }

    protected RepositoryPath getRepositoryPath(final Repository repository)
    {
        return repositoryPathResolver.resolve(repository);
    }

    protected LayoutProvider getRepositoryProvider(final Repository repository)
    {
        return layoutProviderRegistry.getProvider(repository.getLayout());
    }

    protected boolean isOperationDeniedByRoutingRules(final Repository groupRepository,
                                                      final List<Repository> leafRoute,
                                                      final String artifactPath) throws IOException
    {
        for (final Repository leaf : leafRoute)
        {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(leaf).resolve(artifactPath);
            if (artifactRoutingRulesChecker.isDenied(groupRepository, repositoryPath))
            {
                return true;
            }
        }
        return false;
    }

    protected abstract UpdateCallback newInstance(RepositoryPath repositoryPath);

    protected Repository getRepository(final String storageId,
                                       final String repositoryId)
    {
        return configurationManagementService.getConfiguration()
                                             .getStorage(storageId)
                                             .getRepository(repositoryId);
    }

    protected interface UpdateCallback
    {

        default void beforeUpdate()
                throws IOException
        {
            // do nothing, by default
        }

        default void performUpdate(RepositoryPath parentRepositoryArtifactAbsolutePath)
                throws IOException
        {
            // do nothing, by default
        }
    }

    public static class StopUpdateSilentlyException
            extends RuntimeException
    {


    }
}
