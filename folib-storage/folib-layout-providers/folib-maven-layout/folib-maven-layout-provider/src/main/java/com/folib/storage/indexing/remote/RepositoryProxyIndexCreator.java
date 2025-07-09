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
package com.folib.storage.indexing.remote;

import com.folib.storage.indexing.*;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.folib.storage.indexing.RepositoryIndexCreator.RepositoryIndexCreatorQualifier;
import com.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Date;
import java.util.Objects;

import org.apache.maven.index.incremental.DefaultIncrementalHandler;
import org.apache.maven.index.updater.DefaultIndexUpdater;
import org.apache.maven.index.updater.IndexUpdateRequest;
import org.apache.maven.index.updater.IndexUpdateResult;
import org.apache.maven.index.updater.IndexUpdater;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author veadan
 */
@Component
@RepositoryIndexCreatorQualifier(RepositoryTypeEnum.PROXY)
public class RepositoryProxyIndexCreator
        extends AbstractRepositoryIndexCreator
{

    private static final Logger logger = LoggerFactory.getLogger(RepositoryProxyIndexCreator.class);

    private final IndexUpdater indexUpdater = new DefaultIndexUpdater(new DefaultIncrementalHandler(), null);

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ResourceFetcherFactory resourceFetcherFactory;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexingContextFactory indexingContextFactory;

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver;

    @Override
    protected void onIndexingContextCreated(final RepositoryPath repositoryIndexDirectoryPath,
                                            final RepositoryCloseableIndexingContext indexingContext)
            throws IOException
    {
        final Repository repository = indexingContext.getRepositoryRaw();

        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        final IndexUpdateResult updateResult = fetchIndex(indexingContext, repository, storageId, repositoryId);

        final Date contextCurrentTimestamp = indexingContext.getTimestamp();
        if (Objects.equals(updateResult.getTimestamp(), contextCurrentTimestamp))
        {
            logger.info("No update required for remote index {}:{}, as the index is up to date!",
                         storageId, repositoryId);
            if (!IndexPacker.packageExists(repositoryIndexDirectoryPath))
            {
                IndexPacker.pack(repositoryIndexDirectoryPath, indexingContext);
            }
            return;
        }
        if (updateResult.isFullUpdate())
        {
            logger.info("Performed a full index update for {}:{}.", storageId, repositoryId);

        }
        else
        {
            logger.info("Performed an incremental update, with changes covering the period between {} - {}.",
                         contextCurrentTimestamp, updateResult.getTimestamp());
        }
        IndexPacker.pack(repositoryIndexDirectoryPath, indexingContext);
    }

    private IndexUpdateResult fetchIndex(final RepositoryCloseableIndexingContext indexingContext,
                                         final Repository repository,
                                         final String storageId,
                                         final String repositoryId)
            throws IOException
    {
        logger.info("Downloading remote index for {}:{} ...", storageId, repositoryId);

        final IndexUpdateRequest updateRequest = new IndexUpdateRequest(indexingContext,
                                                                        resourceFetcherFactory.createIndexResourceFetcher(
                                                                                indexingContext.getRepositoryUrl(),
                                                                                proxyRepositoryConnectionPoolConfigurationService.getHttpClient()));

        updateRequest.setIndexTempDir(
                RepositoryFiles.temporary(repositoryPathResolver.resolve(repository)).toFile());

        return indexUpdater.fetchAndUpdateIndex(updateRequest);
    }

    @Override
    protected RepositoryIndexingContextFactory getRepositoryIndexingContextFactory()
    {
        return indexingContextFactory;
    }

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return indexDirectoryPathResolver;
    }
}
