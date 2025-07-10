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

import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.RpmRepositoryFeatures;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.repository.Repository;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class RpmUnpublishService{
public enum Result
{
    ARTIFACT_DOES_NOT_EXIST,
    INTERNAL_SERVER_ERROR,
    UNPUBLISHED,
    UNPUBLISH_DISABLED
}

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    private ArtifactResolutionService artifactResolutionService;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RpmRepositoryFeatures repositoryFeatures;

    public Result unpublishPackage(Repository repository,
                                   String packageScope,
                                   String packageName)
    {
        if (!repositoryFeatures.allowsUnpublish(repository.getStorage().getId(), repository.getId()))
        {

            logger.warn(String.format("User tried to 'unpublish' a package [%s], but the feature is disabled",
                    packageName));

            return Result.UNPUBLISH_DISABLED;
        }

        Path packagePath = Paths.get(packageScope, packageName);
        if (packageScope == null)
        {
            packagePath = Paths.get(packageName);
        }
        String repositoryId = repository.getId(), storageId = repository.getStorage().getId();
        RepositoryPath path = null;
        try
        {
            path = artifactResolutionService.resolvePath(storageId, repositoryId,
                    packagePath.toString());
            if (path == null)
            {
                logger.info("Artifact doesn't exist [{}]", path);

                return Result.ARTIFACT_DOES_NOT_EXIST;
            }

            artifactManagementService.delete(path, false);
        }
        catch (IOException e)
        {
            logger.error("Failed to process Npm unpublish a package request: path-[{}]", path, e);

            return Result.INTERNAL_SERVER_ERROR;
        }

        logger.info("Npm unpublish succeeded: path-[{}]", path);

        return Result.UNPUBLISHED;
    }

    public Result unpublishSingleVersion(Repository repository,
                                         String packageScope,
                                         String packageName,
                                         String tarball,
                                         String version)
            throws IllegalArgumentException
    {

        if (!repositoryFeatures.allowsUnpublish(repository.getStorage().getId(), repository.getId()))
        {

            logger.warn(String.format("User tried to 'unpublish' a package [%s], but the feature is disabled",
                    packageName));

            return Result.UNPUBLISH_DISABLED;
        }
        RpmCoordinates coordinates;
        if (packageScope != null)
        {
            coordinates = RpmCoordinates.of(packageName);
        }
        else
        {
            coordinates = RpmCoordinates.of(packageName);
        }
        String repositoryId = repository.getId(), storageId = repository.getStorage().getId();
        RepositoryPath path = null;

        try
        {
            path = artifactResolutionService.resolvePath(storageId, repositoryId, coordinates.buildPath());

            if (path == null)
            {
                logger.info("Artifact doesn't exist [{}]", tarball);

                return Result.ARTIFACT_DOES_NOT_EXIST;
            }

            artifactManagementService.delete(path, false);
            deleteVersionDirectory(path);
        }
        catch (IOException e)
        {
            logger.error("Failed to process Npm unpublish a single version request: path-[{}]", path, e);

            return Result.INTERNAL_SERVER_ERROR;
        }
        logger.info("Npm unpublish succeeded: path-[{}]", path);

        return Result.UNPUBLISHED;
    }

    private void deleteVersionDirectory(Path path)
            throws IOException
    {
        Path versionPath = path.getParent();
        FileUtils.deleteDirectory(versionPath.toFile());
    }
}
