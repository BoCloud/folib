package com.veadan.folib.providers.layout;


import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.storage.repository.HuggingFaceRepositoryFeatures;
import com.veadan.folib.storage.repository.HuggingFaceRepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

@Component("huggingFaceLayoutProvider")
public class HuggingFaceLayoutProvider extends AbstractLayoutProvider<HuggingFaceArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(HuggingFaceLayoutProvider.class);

    public  static final String ALIAS = HuggingFaceArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private HuggingFaceRepositoryManagementStrategy huggingFaceRepositoryManagementStrategy;
    @Inject
    private HuggingFaceRepositoryFeatures huggingFaceRepositoryFeatures;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
        getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public HuggingFaceArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new HuggingFaceArtifactCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    public HuggingFaceRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return huggingFaceRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return huggingFaceRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }
}
