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
package com.folib.storage.indexing;

import com.folib.providers.io.RepositoryPath;
import com.folib.storage.indexing.local.ArtifactEntryJarFileContentsIndexCreator;
import com.folib.storage.indexing.local.ArtifactEntryMinimalArtifactInfoIndexCreator;
import com.folib.storage.repository.Repository;
import com.folib.configuration.MavenRepositoryConfiguration;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.index.context.IndexCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public abstract class AbstractRepositoryIndexingContextFactory
        implements RepositoryIndexingContextFactory
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Override
    public RepositoryCloseableIndexingContext create(final Repository repository)
            throws IOException
    {

        final RepositoryPath indexRepositoryPath = getRepositoryIndexDirectoryPathResolver().resolve(repository);

        final RepositoryCloseableIndexingContext indexingContext = new RepositoryCloseableIndexingContext(
                Indexer.INSTANCE.createIndexingContext(getIndexingContextId(repository),
                                                       repository.getId(),
                                                       indexRepositoryPath.resolve(
                                                               ".cache").toFile(),
                                                       indexRepositoryPath.toFile(),
                                                       getRepositoryUrl(repository),
                                                       null,
                                                       true,
                                                       true,
                                                       getIndexCreators(repository)),
                repository);

        return indexingContext;
    }

    protected String getRepositoryUrl(Repository repository)
    {
        return null;
    }

    protected abstract RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver();

    protected String getIndexingContextId(final Repository repository)
    {
        return repository.getStorage().getId() + ":" + repository.getId();
    }

    protected List<IndexCreator> getIndexCreators(final Repository repository)
    {
        final List<IndexCreator> indexCreators = new ArrayList<>();
        indexCreators.add(ArtifactEntryMinimalArtifactInfoIndexCreator.INSTANCE);
        final MavenRepositoryConfiguration repositoryConfiguration = (MavenRepositoryConfiguration) repository.getRepositoryConfiguration();
        if (repositoryConfiguration != null && repositoryConfiguration.isIndexingClassNamesEnabled())
        {
            indexCreators.add(ArtifactEntryJarFileContentsIndexCreator.INSTANCE);
        }
        return indexCreators;
    }

}
