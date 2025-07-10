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


import cn.hutool.core.io.FileUtil;
import com.folib.artifact.coordinates.GoCoordinates;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.GoRepositoryFeatures;
import com.folib.repository.GoRepositoryStrategy;
import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@Component("goLayoutProvider")
public class GoLayoutProvider extends AbstractLayoutProvider<GoCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(GoLayoutProvider.class);

    public static final String ALIAS = GoCoordinates.LAYOUT_NAME;

    public static final Collection<String> GO_METADATA_SET;

    static {
        HashSet<String> set = new HashSet<>();
        set.add("list");
        set.add("@latest");
        set.add(".info");
        GO_METADATA_SET = Collections.unmodifiableSet(set);
    }

    @Inject
    private GoRepositoryStrategy goRepositoryManagementStrategy;

    @Inject
    private GoRepositoryFeatures goRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public GoCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {

        return GoCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return GO_METADATA_SET.stream().anyMatch(path::endsWith);
    }

    public boolean isArtifact(RepositoryPath path) {
        return "zip".equals(FileUtil.extName(path.getFileName().toString()));
    }


    @Override
    public GoRepositoryStrategy getRepositoryManagementStrategy() {
        return goRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return goRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && !isArtifactMetadata(repositoryPath) && isArtifact(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case EXPIRED:
                    final Instant tenSecondsAgo = Instant.now().minus(60, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || (isArtifactMetadata(repositoryPath)
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    tenSecondsAgo));

                    result.put(attributeType, value);

                    break;
                default:

                    break;
            }
        }

        return result;
    }

}
