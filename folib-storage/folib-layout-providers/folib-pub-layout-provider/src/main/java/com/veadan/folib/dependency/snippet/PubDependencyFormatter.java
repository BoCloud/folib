package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.providers.layout.PubLayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class PubDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    public static final String ALIAS = "pub";

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.info("Initialized the Pub dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return PubLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        PubArtifactCoordinates coordinates = (PubArtifactCoordinates) artifactCoordinates;
        return "dart pub add " + coordinates.getName();
    }

}
