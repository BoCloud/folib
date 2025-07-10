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


import com.folib.artifact.coordinates.HuggingFaceCoordinates;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.HuggingFaceRepositoryFeatures;
import com.folib.storage.repository.HuggingFaceRepositoryStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;

import static com.folib.constant.MlModelConstants.LATEST_LEAD_FILE_NAME;
import static com.folib.constant.MlModelConstants.LEAD_FILE_NAME;

@Component("huggingFaceLayoutProvider")
public class HuggingFaceLayoutProvider extends AbstractLayoutProvider<HuggingFaceCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(HuggingFaceLayoutProvider.class);

    public  static final String ALIAS = HuggingFaceCoordinates.LAYOUT_NAME;

    @Inject
    private HuggingFaceRepositoryStrategy huggingFaceRepositoryManagementStrategy;
    @Inject
    private HuggingFaceRepositoryFeatures huggingFaceRepositoryFeatures;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
        getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public HuggingFaceCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new HuggingFaceCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HuggingFaceRepositoryStrategy getRepositoryManagementStrategy() {
        return huggingFaceRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return huggingFaceRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    public Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
            Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                    attributeTypes);

            for (RepositoryFileAttributeType attributeType : attributeTypes) {
                Object value = result.get(attributeType);
                switch (attributeType) {
                    case ARTIFACT:
                        value = (Boolean) value && !isHfmlArtifact(repositoryPath);

                        if (value != null) {
                            result.put(attributeType, value);
                        }

                        break;
                    case REFRESH_CONTENT:
                        final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                        value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isHfmlPackageJson(repositoryPath)
                                &&
                                !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                        halfAnHourAgo));
                        result.put(attributeType, value);
                        break;
                    default:

                        break;
                }
            }
            return result;
    }

    public boolean isHfmlArtifact(RepositoryPath path) {
        return path.getFileName().toString().lastIndexOf('.')>-1;
    }

    public boolean isHfmlPackageJson(RepositoryPath path) {
        return (LEAD_FILE_NAME.equals(path.getFileName().toString()) || LATEST_LEAD_FILE_NAME.equals(path.getFileName().toString()));
    }
}
