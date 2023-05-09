package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.providers.layout.PypiLayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
* This class is an implementation of DependencySynonymFormatter for Pypi artifacts
*
* @author whalenda
**/
@Component
public class PypiDependencyFormatter
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
        logger.info("Initialized the Pypi dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return PypiLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return PypiLayoutProvider.ALIAS;
    }

    /**
    * This method takes in a set of Pypi Artifact Coordinates and returns the properly formatted dependency snippet
    * @param inputCoordinates PyPiArtifactCoordinates object
    * @return a string representing a properly formatted dependency snippet
    **/
    @Override
    public String getDependencySnippet(ArtifactCoordinates inputCoordinates)
    {
        PypiArtifactCoordinates coordinates = (PypiArtifactCoordinates) inputCoordinates;
        String sb = coordinates.getId(); 

        if (!"version".equals(coordinates.getVersion()))
        {
            sb += " == " + coordinates.getVersion();
        }
        return sb;
    }
}
