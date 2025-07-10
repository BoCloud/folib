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
package com.folib.storage.validation.artifact.version;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author veadan
 */
@Component
public class GenericReleaseVersionValidator
        implements ArtifactCoordinatesValidator
{

    private static final Logger logger = LoggerFactory.getLogger(GenericReleaseVersionValidator.class);

    public static final String ALIAS = "generic-release-version-validator";

    public static final String DESCRIPTION = "Generic release version validator";

    @Inject
    private ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;


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
    public boolean supports(Repository repository)
    {
        return repository.getArtifactCoordinateValidators().contains(ALIAS);
    }

    @Override
    public boolean supports(String layoutProvider)
    {
        return true;
    }

    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        String version = coordinates.getVersion();
        if (isRelease(version) && !repository.isAcceptsReleases())
        {
            throw new VersionValidationException("Cannot deploy a release artifact to a repository which " +
                                                 "does not accept release policy!");
        }
        if (!isRelease(version) && repository.isAcceptsReleases() && !repository.isAcceptsSnapshots())
        {
            throw new VersionValidationException("Cannot deploy a snapshot artifact to a repository with " +
                                                 "a release policy!");
        }
    }

    public boolean isRelease(String version)
    {
        return StringUtils.isNotBlank(version) && !StringUtils.endsWithIgnoreCase(version, "-SNAPSHOT");
    }

}
