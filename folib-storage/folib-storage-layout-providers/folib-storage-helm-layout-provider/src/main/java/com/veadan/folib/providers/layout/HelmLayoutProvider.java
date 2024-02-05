package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.HelmArtifactCoordinates;
import com.veadan.folib.providers.header.HeaderMappingRegistry;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.HelmRepositoryFeatures;
import com.veadan.folib.repository.HelmRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

@Component
public class HelmLayoutProvider  extends AbstractLayoutProvider<HelmArtifactCoordinates> {
    private static final Logger logger = LoggerFactory.getLogger(HelmLayoutProvider.class);

    @Inject
    private HelmRepositoryManagementStrategy helmRepositoryManagementStrategy;

    @Inject
    private HelmRepositoryFeatures helmRepositoryFeatures;

    @Inject
    private HeaderMappingRegistry headerMappingRegistry;

    public static final String ALIAS ="helm";

    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return helmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return helmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return "helm";
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HelmArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return HelmArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
    }

    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

}
