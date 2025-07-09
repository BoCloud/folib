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
package com.folib.providers.layout;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.storage.indexing.RepositoryIndexCreator;
import com.folib.storage.indexing.RepositoryIndexCreator.RepositoryIndexCreatorQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.util.Set;

/**
 * @author veadan
 */
public class MavenFileSystem
        extends LayoutFileSystem
{

    @Inject
    private Maven2LayoutProvider layoutProvider;

    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.HOSTED)
    private RepositoryIndexCreator hostedRepositoryIndexCreator;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.PROXY)
    private RepositoryIndexCreator proxyRepositoryIndexCreator;

    @Inject
    @RepositoryIndexCreatorQualifier(RepositoryTypeEnum.GROUP)
    private RepositoryIndexCreator groupRepositoryIndexCreator;

    public MavenFileSystem(PropertiesBooter propertiesBooter,
                           Repository repository,
                           FileSystem storageFileSystem,
                           LayoutFileSystemProvider provider)
    {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return layoutProvider.getDigestAlgorithmSet();
    }

    public RepositoryPath rebuildIndex(Repository repository)
            throws IOException
    {
        if (!mavenRepositoryFeatures.isIndexingEnabled(repository))
        {
            throw new IndexingDisabledException();
        }
        if (repository.isHostedRepository())
        {
            return hostedRepositoryIndexCreator.apply(repository);
        }
        if (repository.isGroupRepository())
        {
            return groupRepositoryIndexCreator.apply(repository);
        }
        if (repository.isProxyRepository())
        {
            return proxyRepositoryIndexCreator.apply(repository);
        }
        throw new IllegalArgumentException("Repository type not recognized. Index cannot be rebuilt.");
    }

}
