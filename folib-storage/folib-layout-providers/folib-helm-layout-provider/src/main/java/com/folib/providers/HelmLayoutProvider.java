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

import com.folib.artifact.coordinates.HelmCoordinates;
import com.folib.providers.header.HeaderMappingRegistry;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.HelmRepositoryFeatures;
import com.folib.repository.HelmRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

@Component
public class HelmLayoutProvider  extends AbstractLayoutProvider<HelmCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(HelmLayoutProvider.class);

    @Inject
    private HelmRepositoryStrategy helmRepositoryManagementStrategy;

    @Inject
    private HelmRepositoryFeatures helmRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    public static final String ALIAS ="helm";

    @Override
    public RepositoryStrategy getRepositoryManagementStrategy() {
        return helmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return helmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return "helm";
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HelmCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return HelmCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath),repositoryPath.getFileName().toString());
    }

    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isHelmMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isHelmMetadata(repositoryPath);

                    if (value != null) {
                        result.put(attributeType, value);
                    }

                    break;

                default:

                    break;
            }
        }

        return result;
    }

    public boolean isHelmMetadata(RepositoryPath repositoryPath) {
        return !repositoryPath.getFileName().toString().endsWith(".tgz");
    }

}
