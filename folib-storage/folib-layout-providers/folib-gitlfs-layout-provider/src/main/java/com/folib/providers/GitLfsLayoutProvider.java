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
package com.folib.providers;


import com.folib.artifact.coordinates.GitLfsCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.storage.repository.GitLfsRepositoryFeatures;
import com.folib.storage.repository.GitLfsRepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;


@Component("gitLfsLayoutProvider")
public class GitLfsLayoutProvider
        extends AbstractLayoutProvider<GitLfsCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(GitLfsLayoutProvider.class);

    public static final String ALIAS = GitLfsCoordinates.LAYOUT_NAME;

    @Inject
    private GitLfsRepositoryStrategy gitLfsRepositoryManagementStrategy;

    @Inject
    private GitLfsRepositoryFeatures gitLfsRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public GitLfsCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new GitLfsCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public GitLfsRepositoryStrategy getRepositoryManagementStrategy()
    {
        return gitLfsRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return gitLfsRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
