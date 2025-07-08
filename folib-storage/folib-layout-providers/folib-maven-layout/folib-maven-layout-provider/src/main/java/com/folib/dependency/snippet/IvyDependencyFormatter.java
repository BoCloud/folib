package com.folib.dependency.snippet;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.artifact.coordinates.MavenArtifactCoordinates;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class IvyDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    public static final String ALIAS = "Ivy";

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.info("Initialized the Ivy dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return Maven2LayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactCoordinates;

        return "<dependency org=\"" + coordinates.getGroupId() + "\"" +
               " name=\"" + coordinates.getArtifactId() + "\"" +
               " rev=\"" + coordinates.getVersion() + "\" />\n";
    }

}
