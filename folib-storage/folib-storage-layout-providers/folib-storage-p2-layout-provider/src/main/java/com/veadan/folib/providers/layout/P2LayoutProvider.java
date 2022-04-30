package com.veadan.folib.providers.layout;


import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.P2RepositoryFeatures;
import com.veadan.folib.repository.P2RepositoryManagementStrategy;
import com.veadan.folib.artifact.coordinates.P2ArtifactCoordinates;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class P2LayoutProvider
        extends AbstractLayoutProvider<P2ArtifactCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(P2LayoutProvider.class);

    public static final String ALIAS = "P2 Repository";

    @Inject
    private P2RepositoryManagementStrategy p2RepositoryManagementStrategy;

    @Inject
    private P2RepositoryFeatures p2RepositoryFeatures;

    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    protected P2ArtifactCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return P2ArtifactCoordinates.create(RepositoryFiles.relativizePath(path));
    }

    public boolean isArtifactMetadata(RepositoryPath path)
    {
        String fileName = path.getFileName().toString();
        
        return "content.xml".equals(fileName) || "artifacts.xml".equals(fileName) || "artifacts.jar".equals(fileName) ||
                "content.jar".equals(fileName);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return p2RepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public P2RepositoryManagementStrategy getRepositoryManagementStrategy()
    {
        return p2RepositoryManagementStrategy;
    }

}
