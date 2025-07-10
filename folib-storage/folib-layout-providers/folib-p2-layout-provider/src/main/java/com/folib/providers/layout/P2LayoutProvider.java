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


import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.P2RepositoryFeatures;
import com.folib.repository.P2RepositoryStrategy;
import com.folib.artifact.coordinates.P2Coordinates;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class P2LayoutProvider
        extends AbstractLayoutProvider<P2Coordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(P2LayoutProvider.class);

    public static final String ALIAS = "P2 Repository";

    @Inject
    private P2RepositoryStrategy p2RepositoryManagementStrategy;

    @Inject
    private P2RepositoryFeatures p2RepositoryFeatures;

    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public P2Coordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return P2Coordinates.create(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        String fileName = path.getFileName().toString();
        
        return "content.xml".equals(fileName) || "artifacts.xml".equals(fileName) || "artifacts.jar".equals(fileName) ||
                "content.jar".equals(fileName);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return p2RepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public P2RepositoryStrategy getRepositoryManagementStrategy()
    {
        return p2RepositoryManagementStrategy;
    }

}
