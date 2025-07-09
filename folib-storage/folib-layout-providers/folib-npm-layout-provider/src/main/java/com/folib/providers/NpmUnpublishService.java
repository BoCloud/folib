package com.folib.providers;

import com.folib.enums.NpmPacketSuffix;
import com.folib.enums.NpmSubLayout;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.NpmRepositoryFeatures;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author mpoznyak
 */
@Component
public class NpmUnpublishService
{

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
    private NpmRepositoryFeatures repositoryFeatures;

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
        NpmCoordinates coordinates;
        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        if (packageScope != null)
        {
            coordinates = NpmCoordinates.of(
                    String.format("%s/%s", packageScope, packageName), version, packageSuffix);
        }
        else
        {
            coordinates = NpmCoordinates.of(packageName, version,packageSuffix);
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
