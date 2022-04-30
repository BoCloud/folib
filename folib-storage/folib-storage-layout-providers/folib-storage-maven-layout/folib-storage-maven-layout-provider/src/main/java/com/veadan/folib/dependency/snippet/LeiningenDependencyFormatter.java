package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author carlspring
 */
@Component
public class LeiningenDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    public static final String ALIAS = "Leiningen";

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.debug("Initialized the Leiningen dependency formatter.");
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

        return "[" + coordinates.getGroupId() + "/" + coordinates.getArtifactId() + " " +
               "\"" + coordinates.getVersion() +"\"" +
               (coordinates.getExtension() != null && !coordinates.getExtension().equals("jar") ? " :extension \"" + coordinates.getExtension() + "\"" : "") +
               (coordinates.getClassifier() != null ? " :classifier \"" + coordinates.getClassifier() + "\"" : "") +
               "]\n";
    }

}
