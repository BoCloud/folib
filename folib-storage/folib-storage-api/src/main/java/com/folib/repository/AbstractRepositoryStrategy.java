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
package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;

/**
 * @author Veadan
 */
public abstract class AbstractRepositoryStrategy
        implements RepositoryStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractRepositoryStrategy.class);

    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public void createRepository(String storageId,
                                 String repositoryId)
            throws IOException, RepositoryManagementStrategyException
    {
        Storage storage = getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        createRepositoryStructure(repository);
        createRepositoryInternal(storage, getRepository(storageId, repositoryId));
    }

    @Override
    public void createRepositoryStructure(final Repository repository)
            throws IOException
    {
        final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        if (!Files.exists(rootRepositoryPath))
        {
            rootRepositoryPath.getFileSystem().createRootDirectory();
        }
    }

    protected void createRepositoryInternal(Storage storage,
                                            Repository repository)
            throws RepositoryManagementStrategyException
    {
        // override if needed
    }

    protected Storage getStorage(String storageId)
    {
        return getConfiguration().getStorage(storageId);
    }

    protected Repository getRepository(String storageId,
                                       String repositoryId)
    {
        return getStorage(storageId).getRepository(repositoryId);
    }

    @Override
    public void removeRepository(String storageId,
                                 String repositoryId)
            throws IOException
    {
        removeDirectoryStructure(storageId, repositoryId);
    }

    @Override
    public void removeDirectoryStructure(String storageId,
                                         String repositoryId)
            throws IOException
    {
        Repository repository = getRepository(storageId, repositoryId);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);

        if (Files.exists(repositoryPath))
        {
            Files.delete(repositoryPath);

            logger.info("Removed directory structure for repository '{}'.", repositoryPath);
        }
        else
        {
            throw new IOException(String.format("Failed to delete non-existing repository '%s'.", repositoryPath));
        }
        
        repositoryPath.getFileSystem().cleanupRootDirectory();
    }


    protected Configuration getConfiguration()
    {
        return configurationManagementService.getConfiguration();
    }

}
