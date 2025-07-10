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
package com.folib.storage.validation.version;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryPolicyEnum;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author stodorov
 */
@Component
public class MavenSnapshotVersionValidator
        implements MavenVersionValidator
{

    private static final Logger logger = LoggerFactory.getLogger(MavenSnapshotVersionValidator.class);

    public static final String ALIAS = "maven-snapshot-version-validator";

    public static final String DESCRIPTION = "Maven snapshot version validator";

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
        return MavenVersionValidator.super.supports(repository) &&
               RepositoryPolicyEnum.SNAPSHOT.getPolicy().equals(repository.getPolicy());
    }

    /**
     * Matches versions:
     * 1.0-20131004
     * 1.0-20131004.115330
     * 1.0-20131004.115330-1
     * 1.0.8-20151025.032208-1
     * 1.0.8-alpha-1-20151025.032208-1
     */
    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        String version = coordinates.getVersion();
        if (isSnapshot(version) && !repository.isAcceptsSnapshots())
        {
            throw new VersionValidationException("Cannot deploy a SNAPSHOT artifact to a repository with a release policy!");
        }
        if (!isSnapshot(version) && repository.isAcceptsSnapshots() && !repository.isAcceptsReleases())
        {
            throw new VersionValidationException("Cannot deploy a release artifact to a repository with a SNAPSHOT policy!");
        }
    }

}
