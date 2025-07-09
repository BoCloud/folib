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
package com.folib.storage.validation.deployment;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.version.VersionValidationException;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component("redeploymentValidator")
public class RedeploymentValidator
        implements ArtifactDeploymentValidator
{

    private static final Logger logger = LoggerFactory.getLogger(RedeploymentValidator.class);

    public static final String ALIAS = "redeployment-validator";

    public static final String DESCRIPTION = "Re-deployment validator";

    @Lazy
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Lazy
    @Inject
    private ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register()
    {
        artifactCoordinatesValidatorRegistry.addProvider(ALIAS, this);

        logger.info("Registered artifact coordinates validator '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDescription()
    {
        return DESCRIPTION;
    }

    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException,
                   IOException
    {
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        
        if (repository.isAcceptsReleases() &&
            (!repository.isAllowsDeployment() && RepositoryFiles.artifactExists(repositoryPath)))
        {
            throw new VersionValidationException("The " + repository.getStorage().getId() + ":" +
                                                 repository.toString() +
                                                 " repository does not allow artifact re-deployment! (" +
                                                 coordinates.buildPath() + ")");
        }
    }

}
