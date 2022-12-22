package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.ConanRepositoryFeatures;
import com.veadan.folib.repository.ConanRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

@Component
public class ConanLayoutProvider extends AbstractLayoutProvider<ConanArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(ConanLayoutProvider.class);

    @Inject
    private ConanRepositoryManagementStrategy conanRepositoryManagementStrategy;

    @Inject
    private ConanRepositoryFeatures conanRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    public static final String ALIAS = "conan";

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return conanRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return conanRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return "conan";
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    protected ConanArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return ConanArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
    }

    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

}

