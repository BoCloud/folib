package com.folib.providers;


import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.PhpRepositoryFeatures;
import com.folib.repository.PhpRepositoryStrategy;
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
        extends AbstractLayoutProvider<PhpCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(PhpLayoutProvider.class);

    public static final String ALIAS = PhpCoordinates.LAYOUT_NAME;

    @Inject
    private PhpRepositoryStrategy phpRepositoryManagementStrategy;

    @Inject
    private PhpRepositoryFeatures phpRepositoryFeatures;


    @PostConstruct
    public void register() {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public PhpCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException {
        PhpCoordinates phpArtifactCoordinates = PhpCoordinates.parse(RepositoryFiles.relativizePath(path));
        if (Objects.nonNull(path.getArtifactEntry()) && Objects.nonNull(path.getArtifactEntry().getArtifactCoordinates())) {
            phpArtifactCoordinates.setDescription(path.getArtifactEntry().getArtifactCoordinates().getCoordinates().getOrDefault(PhpCoordinates.DESCRIPTION, ""));
        }
        return phpArtifactCoordinates;
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path) {
        return false;
    }


    @Override
    public PhpRepositoryStrategy getRepositoryManagementStrategy() {
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
