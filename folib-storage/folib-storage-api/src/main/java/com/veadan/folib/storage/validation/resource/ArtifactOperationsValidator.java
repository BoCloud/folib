package com.veadan.folib.storage.validation.resource;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.ArtifactResolutionException;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Collections;

/**
 * @author mtodorov
 */
@Slf4j
@Component("artifactOperationsValidator")
public class ArtifactOperationsValidator {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactRepository artifactRepository;

    public ArtifactOperationsValidator() {
    }

    public void validate(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        checkArtifactPath(repositoryPath);

        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();

        checkStorageExists(storage.getId());
        checkRepositoryExists(storage.getId(), repository.getId());
    }

    public void checkStorageExists(String storageId)
            throws ArtifactResolutionException {
        if (storageId == null) {
            throw new ArtifactResolutionException("No storage specified.");
        }

        if (getConfiguration().getStorage(storageId) == null) {
            throw new ArtifactResolutionException("Storage " + storageId + " does not exist.");
        }
    }

    public void checkRepositoryExists(String storageId,
                                      String repositoryId)
            throws ArtifactResolutionException {
        if (repositoryId == null) {
            throw new ArtifactResolutionException("No repository specified.");
        }

        if (getConfiguration().getStorage(storageId)
                .getRepository(repositoryId) == null) {
            throw new ArtifactResolutionException("Repository " + repositoryId + " does not exist.");
        }
    }

    public void checkArtifactPath(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        if (repositoryPath == null) {
            throw new ArtifactResolutionException("No artifact path specified.");
        }
    }

    public void checkAllowsDeployment(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeployment() ||
                RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) ||
                RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            // It should not be possible to write artifacts to:
            // - a repository that doesn't allow the deployment of artifacts
            // - a proxy repository
            // - a group repository
            //
            // NOTE:
            // - A proxy repository should only serve artifacts that already exist in the cache, or the remote host.
            // - Both the ProxyRepositoryProvider and GroupRepositoryProvider need to have an implementation of the
            //   getOutputStream(...) method, which is why this check is performed here instead.

            throw new ArtifactStorageException("Deployment of artifacts to " + repository.getType() +
                    " repositories is not allowed!");
        }
    }

    public void checkAllowsRedeployment(Repository repository,
                                        ArtifactCoordinates coordinates)
            throws IOException,
            ProviderImplementationException {
        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        if (RepositoryFiles.artifactExists(repositoryPath) && !repository.isAllowsRedeployment()) {
            throw new ArtifactStorageException("Re-deployment of artifacts to " +
                    repository.getStorage().getId() + ":" + repository.getId() +
                    " repository is not allowed!");
        }
    }

    public void checkAllowsDeletion(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeletion()) {
            throw new ArtifactStorageException("Deleting artifacts from " + repository.getType() +
                    " repository is not allowed!");
        }
    }

    public void checkArtifactSize(String storageId,
                                  String repositoryId,
                                  MultipartFile uploadedFile)
            throws ArtifactResolutionException {
        if (uploadedFile.isEmpty() || uploadedFile.getSize() == 0) {
            throw new ArtifactResolutionException("Uploaded file is empty.");
        }

        Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryId);
        long artifactMaxSize = repository.getArtifactMaxSize();

        if (artifactMaxSize > 0 && uploadedFile.getSize() > artifactMaxSize) {
            throw new ArtifactResolutionException("The size of the artifact exceeds the maximum size accepted by " +
                    "this repository (" + uploadedFile.getSize() + "/" +
                    artifactMaxSize + ").");
        }
    }


    public void checkRepositorySize(RepositoryPath repositoryPath)
            throws IOException {
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        Repository repository = repositoryPath.getRepository();
        long repositoryMaxSize = repository.getRepositoryMaxSize();
        if (repositoryMaxSize <= 0) {
            return;
        }
        long repositoryBytesSize = artifactRepository.artifactsBytesStatistics(Collections.singletonList(String.format("%s-%s", storageId, repositoryId)));
        BigDecimal repositoryMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(repositoryMaxSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal repositoryRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(repositoryBytesSize, FileUnitTypeEnum.TB.getUnit());
        if (repositoryRealTbSize.compareTo(repositoryMaxTbSize) >= 0) {
            throw new ArtifactResolutionException(String.format("The size of the repository [%s] exceeds the maximum size accepted by " +
                    "this repository (%s/%s) unit %s.", repositoryPath, repositoryRealTbSize, repositoryMaxTbSize, FileUnitTypeEnum.TB.getUnit()));
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
