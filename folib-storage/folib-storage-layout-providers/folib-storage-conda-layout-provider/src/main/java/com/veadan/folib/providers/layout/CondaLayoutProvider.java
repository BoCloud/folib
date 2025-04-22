package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.index.cache.CondaIndexCache;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.CondaRepositoryFeatures;
import com.veadan.folib.repository.CondaRepositoryManagementStrategy;
import com.veadan.folib.services.CondaRepoDataService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.stereotype.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author LingengMa
 * @date 2025-04-02 13:28
 */
@Component("condaLayoutProvider")
public class CondaLayoutProvider extends AbstractLayoutProvider<CondaArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CondaLayoutProvider.class);

    public static final String ALIAS = CondaArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private CondaRepositoryManagementStrategy condaRepositoryManagementStrategy;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private CondaRepositoryFeatures condaRepositoryFeatures;

    @Inject
    private CondaRepoDataService condaRepoDataService;

    @Inject
    private CondaIndexCache condaIndexCache;

    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public CondaArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {

        return CondaArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return path.getFileName().toString().endsWith("index.json");
    }

    public boolean isCondaRepoData(RepositoryPath path) {
        return path.getFileName().toString().endsWith("repodata.json") ||
               path.getFileName().toString().endsWith("current_repodata.json");
    }


    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        if (attributeTypes == null || attributeTypes.length == 0) {
            return super.getRepositoryFileAttributes(repositoryPath, attributeTypes);
        }

        Map<RepositoryFileAttributeType, Object> result = new ConcurrentHashMap<>(super.getRepositoryFileAttributes(repositoryPath, attributeTypes));

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);

            switch (attributeType) {
                case REFRESH_CONTENT:
                    try {
                        if (value instanceof Boolean) {
                            Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                            boolean refreshContentValue = BooleanUtils.isTrue((Boolean) value) ||
                                    (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isIndex(repositoryPath)) &&
                                            !RepositoryFiles.wasModifiedAfter(repositoryPath, halfAnHourAgo);

                            if (refreshContentValue) {
                                condaIndexCache.reset(repositoryPath.toString());
                            }
                            result.put(attributeType, refreshContentValue);
                        }
                    } catch (Exception e) {
                        // Log the exception or handle it appropriately
                        logger.error("Error processing REFRESH_CONTENT attribute", e);
                        throw new IOException("Error processing REFRESH_CONTENT attribute", e);
                    }
                    break;
                default:
                    break;
            }
        }

        return result;
    }


    private boolean isIndex(RepositoryPath repositoryPath) {
        if (repositoryPath == null || repositoryPath.getPath() == null || repositoryPath.getPath().isEmpty()) {
            return false;
        }
        String path = repositoryPath.getPath();
        return path.endsWith("repodata.json") || path.endsWith("current_repodata.json");
    }

    @Override
    public CondaRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return condaRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return condaRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }
}
