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

import com.folib.storage.indexing.AbstractRepositoryIndexingContextFactory;
import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;

import jakarta.inject.Inject;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.REMOTE)
public class RepositoryRemoteIndexingContextFactory
        extends AbstractRepositoryIndexingContextFactory
{

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver;

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return indexDirectoryPathResolver;
    }

    @Override
    protected String getRepositoryUrl(final Repository repository)
    {
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            logger.warn("Repository [{}:{}] was expected to have remote repository provided but was null.",
                        repository.getStorage().getId(), repository.getId());
            return null;

        }
        return remoteRepository.getUrl();
    }
}
