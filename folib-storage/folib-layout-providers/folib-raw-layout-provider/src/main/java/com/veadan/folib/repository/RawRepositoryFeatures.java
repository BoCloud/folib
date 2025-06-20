package com.veadan.folib.repository;

import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.locator.handlers.RemoveRawArtifactOperation;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * @author Veadan
 */
@Slf4j
@Component
public class RawRepositoryFeatures
        implements RepositoryFeatures {

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    public void removeRawArtifact(String storageId,
                                  String repositoryId,
                                  String artifactPath,
                                  int numberToKeep,
                                  Map<String, String> cleanupArtifactPathMap)
            throws IOException {
        Storage storage = getConfiguration().getStorage(storageId);
        if (Objects.isNull(storage)) {
            log.warn("Storage [{}] not found", storageId);
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            log.warn("Repository [{}] [{}] not found", storageId, repositoryId);
            return;
        }
        if (Boolean.FALSE.equals(repository.getEnableCustomLayout())) {
            log.warn("Repository [{}] [{}] custom layout not enabled", storageId, repositoryId);
            return;
        }
        if (StringUtils.isBlank(repository.getCustomLayout())) {
            log.warn("Repository [{}] [{}] custom layout not found", storageId, repositoryId);
            return;
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);

        RemoveRawArtifactOperation operation = new RemoveRawArtifactOperation();
        operation.setBasePath(repositoryPath);
        operation.setNumberToKeep(numberToKeep);
        operation.setCleanupArtifactPathMap(cleanupArtifactPathMap);
        operation.execute(repositoryPath);
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
