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

import com.folib.artifact.coordinates.CocoapodsCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.CocoapodsRepositoryFeatures;
import com.folib.repository.CocoapodsRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import com.folib.util.CocoapodsArtifactUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Set;

/**
 * @author veadan
 * @date 2023/8/2 15:09
 */
@Component
public class CocoapodsLayoutProvider extends AbstractLayoutProvider<CocoapodsCoordinates>
{
    private static final Logger logger = LoggerFactory.getLogger(CocoapodsLayoutProvider.class);
    
    
    public static final String ALIAS = CocoapodsCoordinates.LAYOUT_NAME;
    
    @Inject
    private CocoapodsRepositoryStrategy cocoapodsRepositoryManagementStrategy;
    
    @Inject
    private CocoapodsRepositoryFeatures cocoapodsRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS );
    }
    
    @Override
    public RepositoryStrategy getRepositoryManagementStrategy() {
        return cocoapodsRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return cocoapodsRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public CocoapodsCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException
    {
        final String relativizePath = RepositoryFiles.relativizePath(repositoryPath);

        CocoapodsCoordinates coordinates = new CocoapodsCoordinates(relativizePath);

        if (null != repositoryPath.getArtifactEntry())
        {
            final CocoapodsCoordinates artifactCoordinates = (CocoapodsCoordinates) repositoryPath.getArtifactEntry().getArtifactCoordinates();
            final String path = artifactCoordinates.getPath();
            final String version = artifactCoordinates.getVersion();
            final String baseName = artifactCoordinates.getBaseName();
            if (StringUtils.isNotBlank(path))
            { coordinates.setPath(path); }
            if (StringUtils.isNotBlank(version))
            { coordinates.setVersion(version); }
            if (StringUtils.isNotBlank(baseName))
            { coordinates.setBaseName(baseName); }
        }
        if (relativizePath.endsWith("tar.gz"))
        {
            if (StringUtils.isEmpty(coordinates.getBaseName()) || StringUtils.isEmpty(coordinates.getVersion())) 
            { // 如发现制品信息不全，尝试从制品包里读取数据
                if (Files.exists(repositoryPath)) 
                {
                    try (final InputStream inputStream = Files.newInputStream(repositoryPath)) {
                        final String podspecSourceContent = CocoapodsArtifactUtil.fetchPodspecSourceContentByInputStream(inputStream);
                        final CocoapodsArtifactUtil.PodSpec podSpec = CocoapodsArtifactUtil.resolvePodSpec(podspecSourceContent);
                        if (null != podSpec)
                        {
                            coordinates.setBaseName(podSpec.getName());
                            coordinates.setVersion(podSpec.getVersion());
                        }
                    }
                }
            }
        }
        
        return coordinates;
    }
}
