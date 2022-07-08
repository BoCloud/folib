package com.veadan.folib.providers.layout;


import java.io.IOException;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.artifact.coordinates.RawArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RawRepositoryFeatures;
import com.veadan.folib.repository.RawRepositoryManagementStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("rawLayoutProvider")
public class RawLayoutProvider
        extends AbstractLayoutProvider<RawArtifactCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(RawLayoutProvider.class);

    public static final String ALIAS = RawArtifactCoordinates.LAYOUT_NAME;

    @Inject
    private RawRepositoryManagementStrategy rawRepositoryManagementStrategy;

    @Inject
    private RawRepositoryFeatures rawRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    protected RawArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new RawArtifactCoordinates(RepositoryFiles.relativizePath(path));
    }

    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public RawRepositoryManagementStrategy getRepositoryManagementStrategy()
    {
        return rawRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return rawRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

}
