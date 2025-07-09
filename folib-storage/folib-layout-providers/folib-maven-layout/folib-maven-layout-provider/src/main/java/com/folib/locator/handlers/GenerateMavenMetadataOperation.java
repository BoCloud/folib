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
package com.folib.locator.handlers;

import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.metadata.MavenMetadataManager;
import com.folib.storage.metadata.VersionCollectionRequest;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class GenerateMavenMetadataOperation
        extends AbstractMavenArtifactLocatorOperation
{

    private static final Logger logger = LoggerFactory.getLogger(GenerateMavenMetadataOperation.class);

    private final MavenMetadataManager mavenMetadataManager;

    private final ArtifactEventListenerRegistry artifactEventListenerRegistry;


    public GenerateMavenMetadataOperation(@Nonnull final MavenMetadataManager mavenMetadataManager,
                                          @Nonnull final ArtifactEventListenerRegistry artifactEventListenerRegistry)
    {
        Objects.requireNonNull(mavenMetadataManager);
        Objects.requireNonNull(artifactEventListenerRegistry);
        this.mavenMetadataManager = mavenMetadataManager;
        this.artifactEventListenerRegistry = artifactEventListenerRegistry;
    }

    @Override
    public void executeOperation(VersionCollectionRequest request,
                                 RepositoryPath artifactGroupDirectoryPath,
                                 List<RepositoryPath> versionDirectories)
    {
        try
        {
            mavenMetadataManager.generateMetadata(artifactGroupDirectoryPath, request);
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(artifactGroupDirectoryPath.resolve("maven-metadata.xml"));
        }
        catch (Exception e)
        {
            logger.error("Failed to generate metadata for {}", artifactGroupDirectoryPath, e);
        }
    }

}
