package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 * @date 1/9/2024 18:42
 */
@Component
public class GoMODDependencyFormatter implements DependencySynonymFormatter{

    private static final Logger logger = LoggerFactory.getLogger(GoMODDependencyFormatter.class);

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.info("Initialized the Go mod dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return GoLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return GoLayoutProvider.ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        GoArtifactCoordinates coordinates = (GoArtifactCoordinates) artifactCoordinates;
        return "go get "+coordinates.getName()+"@"+ coordinates.getVersion();
    }
}
