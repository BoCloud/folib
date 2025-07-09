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
package com.folib.booters;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import jakarta.inject.Inject;

import com.folib.configuration.ConfigurationManager;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.repository.group.GroupRepositorySetCollector;
import com.folib.repository.RepositoryManagementStrategyException;
import com.folib.services.RepositoryManagementService;
import com.folib.configuration.Configuration;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryStatusEnum;
import com.folib.util.ThrowingConsumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.lock.FencedLock;

/**
 * @author veadan
 */
public class StorageBooter
{

    private static final Logger logger = LoggerFactory.getLogger(StorageBooter.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    private GroupRepositorySetCollector groupRepositorySetCollector;

    @Inject
    private PropertiesBooter propertiesBooter;

    @Inject
    private HazelcastInstance hazelcastInstance;

    public StorageBooter()
    {
    }

    //禁用存储空间、仓库启动初始化
//    @PostConstruct
    public void initialize()
            throws IOException, RepositoryManagementStrategyException
    {
        FencedLock lock = hazelcastInstance.getCPSubsystem().getLock("StorageBooterLock");
        if (lock.tryLock())
        {
            try
            {
                Optional.of(System.getProperty("java.io.tmpdir"))
                        .map(Paths::get)
                        .filter(Path::isAbsolute)
                        .ifPresent(ThrowingConsumer.unchecked(Files::createDirectories));
                
                Configuration configuration = configurationManager.getConfiguration();
                initializeStorages(configuration.getStorages());
                Collection<Repository> repositories = getRepositoriesHierarchy(configuration.getStorages());
                if (!repositories.isEmpty())
                {
                    logger.info(" -> Initializing repositories...");
                }
                repositories.forEach(ThrowingConsumer.unchecked(this::initializeRepository));
            }
            finally
            {
                lock.unlock();
            }
        }
        else
        {
            logger.info("Failed to initialize the repositories. Another JVM may have already done this.");
        }
    }


    private void initializeStorages(final Map<String, Storage> storages)
            throws IOException
    {
        logger.info("Running folib storage booter...");
        logger.info(" -> Creating storage directory skeleton...");

        for (Map.Entry<String, Storage> stringStorageEntry : storages.entrySet())
        {
            initializeStorage(stringStorageEntry.getValue());
        }

    }

    private void initializeStorage(Storage storage)
            throws IOException
    {
        logger.info("  * Initializing {}...", storage.getId());
    }

    private void initializeRepository(Repository repository)
            throws IOException, RepositoryManagementStrategyException
    {
        logger.info("  * Initializing {}:{}...", repository.getStorage().getId(), repository.getId());

        if (layoutProviderRegistry.getProvider(repository.getLayout()) == null)
        {
            logger.error("Failed to resolve layout [{}] for repository [{}].",
                         repository.getLayout(),
                         repository.getId());
            return;
        }

        repositoryManagementService.createRepository(repository.getStorage().getId(), repository.getId());

        if (RepositoryStatusEnum.IN_SERVICE.getStatus().equals(repository.getStatus()))
        {
            repositoryManagementService.putInService(repository.getStorage().getId(), repository.getId());
        }
    }

    private Collection<Repository> getRepositoriesHierarchy(final Map<String, Storage> storages)
    {
        final Map<String, Repository> repositoriesHierarchy = new LinkedHashMap<>();
        for (final Storage storage : storages.values())
        {
            for (final Repository repository : storage.getRepositories().values())
            {
                addRepositoriesByChildrenFirst(repositoriesHierarchy, repository);
            }
        }

        return repositoriesHierarchy.values();
    }

    private void addRepositoriesByChildrenFirst(final Map<String, Repository> repositoriesHierarchy,
                                                final Repository repository)
    {
        if (!repository.isGroupRepository())
        {
            repositoriesHierarchy.putIfAbsent(repository.getId(), repository);

            return;
        }
        groupRepositorySetCollector.collect(repository, true)
                                   .stream().forEach(r -> addRepositoriesByChildrenFirst(repositoriesHierarchy, r));

        repositoriesHierarchy.putIfAbsent(repository.getId(), repository);
    }

    public RepositoryManagementService getRepositoryManagementService()
    {
        return repositoryManagementService;
    }

    public void setRepositoryManagementService(RepositoryManagementService repositoryManagementService)
    {
        this.repositoryManagementService = repositoryManagementService;
    }

}
