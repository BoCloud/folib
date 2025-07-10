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

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.locator.ArtifactDirectoryLocator;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.locator.handlers.RemoveMavenArtifactOperation;
import com.folib.locator.handlers.RemoveTimestampedSnapshotOperation;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.metadata.MavenArtifactManager;
import com.folib.storage.metadata.MavenSnapshotManager;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryPolicyEnum;
import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import com.folib.storage.validation.version.MavenReleaseVersionValidator;
import com.folib.storage.validation.version.MavenSnapshotVersionValidator;
import com.folib.configuration.MavenRepositoryConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author Veadan
 */
@Component
public class MavenRepositoryFeatures
        implements RepositoryFeatures {

    private static final Logger logger = LoggerFactory.getLogger(MavenRepositoryFeatures.class);

    public static final String INDEX = ".index";

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private MavenSnapshotManager mavenSnapshotManager;

    @Inject
    private MavenArtifactManager mavenArtifactManager;

    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Inject
    private MavenReleaseVersionValidator mavenReleaseVersionValidator;

    @Inject
    private MavenSnapshotVersionValidator mavenSnapshotVersionValidator;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    protected ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init() {
        defaultArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                mavenReleaseVersionValidator.getAlias(),
                mavenSnapshotVersionValidator.getAlias()));
    }

    public void removeTimestampedSnapshots(String storageId,
                                           String repositoryId,
                                           String artifactPath,
                                           int numberToKeep,
                                           int keepPeriod)
            throws IOException {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (repository.getPolicy().equals(RepositoryPolicyEnum.SNAPSHOT.getPolicy()) || repository.getPolicy().equals(RepositoryPolicyEnum.MIXED.getPolicy())) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);

            RemoveTimestampedSnapshotOperation operation = new RemoveTimestampedSnapshotOperation(mavenSnapshotManager);
            operation.setBasePath(repositoryPath);
            operation.setNumberToKeep(numberToKeep);
            operation.setKeepPeriod(keepPeriod);

            ArtifactDirectoryLocator locator = new ArtifactDirectoryLocator();
            locator.setOperation(operation);
            locator.locateArtifactDirectories();
        } else {
            throw new ArtifactStorageException("Type of repository is invalid: repositoryId - " + repositoryId);
        }
    }

    public void removeMavenArtifact(String storageId,
                                           String repositoryId,
                                           String artifactPath,
                                           int numberToKeep,
                                           int keepPeriod)
            throws IOException {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);

        RemoveMavenArtifactOperation operation = new RemoveMavenArtifactOperation(mavenArtifactManager);
        operation.setBasePath(repositoryPath);
        operation.setNumberToKeep(numberToKeep);
        operation.setKeepPeriod(keepPeriod);

        ArtifactDirectoryLocator locator = new ArtifactDirectoryLocator();
        locator.setOperation(operation);
        locator.locateArtifactDirectories();
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    public boolean isIndexingEnabled(Repository repository) {
        MavenRepositoryConfiguration repositoryConfiguration = (MavenRepositoryConfiguration) repository.getRepositoryConfiguration();
        return repositoryConfiguration != null && repositoryConfiguration.isIndexingEnabled();
    }

    public void versionValidator(RepositoryPath repositoryPath) throws Exception {
        try {
            Repository repository = repositoryPath.getFileSystem().getRepository();
            if (!RepositoryFiles.isArtifact(repositoryPath)) {
                return;
            }
            ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);
            Set<String> versionValidatorSets = new LinkedHashSet<>(Arrays.asList(
                    mavenReleaseVersionValidator.getAlias(),
                    mavenSnapshotVersionValidator.getAlias()));
            for (String validatorKey : versionValidatorSets) {
                ArtifactCoordinatesValidator validator = artifactCoordinatesValidatorRegistry.getProvider(
                        validatorKey);
                if (validator.supports(repository)) {
                    validator.validate(repository, coordinates);
                }
            }
        } catch (IOException io) {
            throw new RuntimeException(io);
        } catch (VersionValidationException e) {
            throw new ArtifactStorageException(e);
        }
    }

}
