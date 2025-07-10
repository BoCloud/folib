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
package com.folib.storage.indexing.group;

import com.folib.storage.indexing.*;
import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.configuration.ConfigurationUtils;
import com.folib.storage.Storage;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.List;

import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.store.SimpleFSDirectory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.GROUP)
public class RepositoryGroupIndexCreator
        extends AbstractRepositoryIndexCreator
{

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexDirectoryPathResolver localIndexDirectoryPathResolver;

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexDirectoryPathResolver remoteIndexDirectoryPathResolver;

    @Inject
    @RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexingContextFactory indexingContextFactory;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    protected void onIndexingContextCreated(final RepositoryPath repositoryIndexDirectoryPath,
                                            final RepositoryCloseableIndexingContext indexingContext)
            throws IOException
    {

        indexingContext.purge();
        mergeSubrepositoryIndexes(indexingContext);
        IndexPacker.pack(repositoryIndexDirectoryPath, indexingContext);
    }

    private void mergeSubrepositoryIndexes(RepositoryCloseableIndexingContext indexingContext)
            throws IOException
    {
        final Repository repository = indexingContext.getRepositoryRaw();
        final Storage storage = repository.getStorage();
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (final String storageAndRepositoryId : storageAndRepositoryIdList)
        {
            final String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
            final String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);

            final RepositoryPath subRepositoryIndexDirectoryPath = getSubRepositoryIndexPath(sId, rId);

            if (repositoryPathLock.lock(subRepositoryIndexDirectoryPath)) {
                try
                {
                    try
                    {
                        indexingContext.merge(new SimpleFSDirectory(subRepositoryIndexDirectoryPath));
                    }
                    catch (IndexNotFoundException ex)
                    {
                        logger.warn("IndexNotFound in [{}]", subRepositoryIndexDirectoryPath, ex);
                    }
                }
                finally
                {
                    repositoryPathLock.unLock(subRepositoryIndexDirectoryPath);
                }
            }
        }
    }

    private RepositoryPath getSubRepositoryIndexPath(final String storageId,
                                                     final String repositoryId)
    {
        final Repository repository = configurationManager.getRepository(storageId, repositoryId);

        final RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver =
                repository.isProxyRepository() ? remoteIndexDirectoryPathResolver :
                localIndexDirectoryPathResolver;

        return indexDirectoryPathResolver.resolve(repository);
    }

    @Override
    protected RepositoryIndexingContextFactory getRepositoryIndexingContextFactory()
    {
        return indexingContextFactory;
    }

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return localIndexDirectoryPathResolver;
    }
}

