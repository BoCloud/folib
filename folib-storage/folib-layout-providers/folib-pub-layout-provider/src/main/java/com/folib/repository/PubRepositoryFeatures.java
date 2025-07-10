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
package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

@Component
public class PubRepositoryFeatures implements RepositoryFeatures {

    private static final int CHANGES_BATCH_SIZE = 500;

    private static final boolean ALLOWS_UN_PUBLISH_DEFAULT = true;

    private static final Logger logger = LoggerFactory.getLogger(PubRepositoryFeatures.class);

    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Lazy
    @Inject
    private RedeploymentValidator redeploymentValidator;
    @Lazy
    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;
    @Lazy
    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init() {
        defaultArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                genericReleaseVersionValidator.getAlias(),
                genericSnapshotVersionValidator.getAlias()));
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
