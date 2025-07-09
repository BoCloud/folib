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


import java.io.IOException;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.artifact.coordinates.RawCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.RawRepositoryFeatures;
import com.folib.repository.RawRepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("rawLayoutProvider")
public class RawLayoutProvider
        extends AbstractLayoutProvider<RawCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(RawLayoutProvider.class);

    public static final String ALIAS = RawCoordinates.LAYOUT_NAME;

    @Inject
    private RawRepositoryStrategy rawRepositoryManagementStrategy;

    @Inject
    private RawRepositoryFeatures rawRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public RawCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new RawCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public RawRepositoryStrategy getRepositoryManagementStrategy()
    {
        return rawRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return rawRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
