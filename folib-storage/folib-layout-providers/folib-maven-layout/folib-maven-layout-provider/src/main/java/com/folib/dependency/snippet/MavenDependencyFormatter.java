package com.folib.dependency.snippet;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.folib.providers.layout.Maven2LayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class MavenDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.info("Initialized the Maven dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return Maven2LayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return Maven2LayoutProvider.ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactCoordinates;

        String sb = "<dependency>\n" +
                    "    <groupId>" + coordinates.getGroupId() + "</groupId>\n" +
                    "    <artifactId>" + coordinates.getArtifactId() + "</artifactId>\n" +
                    "    <version>" + coordinates.getVersion() + "</version>\n" +
                    "    <type>" + coordinates.getExtension() + "</type>\n" +
                    (coordinates.getClassifier() != null ?
                    "    <classifier>" + coordinates.getClassifier() + "</classifier>\n" : "" )  +
                    "    <scope>compile</scope>\n" +
                    "</dependency>\n";

        return sb;
    }

}
