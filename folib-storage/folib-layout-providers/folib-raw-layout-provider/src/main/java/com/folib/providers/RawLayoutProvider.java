package com.folib.providers;


import java.io.IOException;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.artifact.coordinates.RawCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.RawRepositoryFeatures;
import com.folib.repository.RawRepositoryStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("rawLayoutProvider")
public class RawLayoutProvider
        extends AbstractLayoutProvider<RawCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(RawLayoutProvider.class);

    public static final String ALIAS = RawCoordinates.LAYOUT_NAME;

    @Inject
    private RawRepositoryStrategy rawRepositoryManagementStrategy;

    @Inject
    private RawRepositoryFeatures rawRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public RawCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return new RawCoordinates(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return false;
    }


    @Override
    public RawRepositoryStrategy getRepositoryManagementStrategy()
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
