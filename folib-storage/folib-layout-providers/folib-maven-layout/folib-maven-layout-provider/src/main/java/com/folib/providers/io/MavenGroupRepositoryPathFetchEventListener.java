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
package com.folib.providers.io;

import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.event.AsyncEventListener;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.repository.GroupRepositoryProvider;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.providers.repository.event.GroupRepositoryPathFetchEvent;
import com.folib.services.support.ArtifactRoutingRulesChecker;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.FutureTask;

/**
 * @author veadan
 */
@Component
public class MavenGroupRepositoryPathFetchEventListener {

    private static final Logger logger = LoggerFactory.getLogger(MavenGroupRepositoryPathFetchEventListener.class);

    @Inject
    private Maven2LayoutProvider maven2LayoutProvider;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactRoutingRulesChecker artifactRoutingRulesChecker;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @AsyncEventListener
    public void handle(final GroupRepositoryPathFetchEvent event)
            throws IOException {
        RepositoryPath repositoryPath = event.getPath();
        if (!Maven2LayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout())) {
            return;
        }

        if (!maven2LayoutProvider.requiresGroupAggregation(repositoryPath)) {
            return;
        }

        fetchInSubRepositories(repositoryPath);
    }

    /**
     * @see GroupRepositoryProvider#resolvePathTraversal(RepositoryPath)
     */
    private void fetchInSubRepositories(final RepositoryPath repositoryPath)
            throws IOException {
        Repository groupRepository = repositoryPath.getRepository();
        Storage storage = groupRepository.getStorage();
        List<FutureTask<Path>> futureTasks = Lists.newArrayList();
        FutureTask<Path> futureTask = null;
        MavenGroupRepositoryPathFetchTask mavenGroupRepositoryPathFetchTask = null;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(groupRepository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManager.getRepository(sId, rId);

            if (!subRepository.isInService()) {
                continue;
            }

            RepositoryPath resolvedPath = repositoryPathResolver.resolve(subRepository, repositoryPath);
            if (artifactRoutingRulesChecker.isDenied(groupRepository, resolvedPath)) {
                continue;
            }

            RepositoryProvider provider = repositoryProviderRegistry.getProvider(subRepository.getType());
            mavenGroupRepositoryPathFetchTask = new MavenGroupRepositoryPathFetchTask(provider, resolvedPath);
            futureTask = new FutureTask<>(mavenGroupRepositoryPathFetchTask);
            futureTasks.add(futureTask);
            asyncFetchRemotePackageThreadPoolTaskExecutor.submit(futureTask);
        }

        fetchPathsInTask(futureTasks);
    }

    private void fetchPathsInTask(final List<FutureTask<Path>> futureTasks) {
        futureTasks
                .forEach(action -> {
                    try {
                        action.get();
                    } catch (Exception e) {
                        logger.error(e.getMessage(), e);
                    }
                });
    }
}
