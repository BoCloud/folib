package com.veadan.folib.providers.layout;


import com.veadan.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.PhpRepositoryFeatures;
import com.veadan.folib.repository.PhpRepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;

/**
 * @author Veadan
 */
@Component("phpLayoutProvider")
public class PhpLayoutProvider
        extends AbstractLayoutProvider<PhpArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(PhpLayoutProvider.class);

    public static final String ALIAS = PhpArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private PhpRepositoryManagementStrategy phpRepositoryManagementStrategy;

    @Inject
    private PhpRepositoryFeatures phpRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PhpArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        PhpArtifactCoordinates phpArtifactCoordinates = PhpArtifactCoordinates.parse(RepositoryFiles.relativizePath(path));
        if (Objects.nonNull(path.getArtifactEntry()) && Objects.nonNull(path.getArtifactEntry().getArtifactCoordinates())) {
            phpArtifactCoordinates.setDescription(path.getArtifactEntry().getArtifactCoordinates().getCoordinates().getOrDefault(PhpArtifactCoordinates.DESCRIPTION, ""));
        }
        return phpArtifactCoordinates;
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return false;
    }


    @Override
    public PhpRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return phpRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return phpRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

}
