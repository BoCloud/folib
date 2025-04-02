package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.CondaRepositoryFeatures;
import com.veadan.folib.repository.CondaRepositoryManagementStrategy;
import org.springframework.stereotype.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.*;

/**
 * @author LingengMa
 * @date 2025-04-02 13:28
 */
@Component("condaLayoutProvider")
public class CondaLayoutProvider extends AbstractLayoutProvider<CondaArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CondaLayoutProvider.class);

    public static final String ALIAS = CondaArtifactCoordinates.LAYOUT_NAME;

    public static final Collection<String> CONDA_METADATA_SET;

    static {
        HashSet<String> set = new HashSet<>();
        set.add("list");
        set.add("@latest");
        set.add(".info");
        CONDA_METADATA_SET = Collections.unmodifiableSet(set);
    }

    @Inject
    private CondaRepositoryManagementStrategy condaRepositoryManagementStrategy;

    @Inject
    private CondaRepositoryFeatures condaRepositoryFeatures;

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
        return CONDA_METADATA_SET.stream().anyMatch(path::endsWith);
    }


    public boolean isArtifact(RepositoryPath path) {
        //TODO
        return true;
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

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
        return new HashMap<>();
    }
}
