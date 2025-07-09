package com.folib.dependency;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.folib.dependency.snippet.DependencySynonymFormatter;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.providers.NugetLayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class NugetDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    public static final String ALIAS = "NuGet";

    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;


    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);

        logger.info("Initialized the NuGet dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return NugetLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        NugetCoordinates coordinates = (NugetCoordinates) artifactCoordinates;

        String sb = "<dependency id=\"" + coordinates.getId() + "\"" +
                    " version=\"" + coordinates.getVersion() + "\" />\n";

        return sb;
    }

}
