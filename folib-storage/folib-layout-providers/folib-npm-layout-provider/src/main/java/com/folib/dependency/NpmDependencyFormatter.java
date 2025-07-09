package com.folib.dependency;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.folib.dependency.snippet.DependencySynonymFormatter;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.providers.NpmLayoutProvider;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class NpmDependencyFormatter
        implements DependencySynonymFormatter
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    public static final String ALIAS = "npm";

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
        return NpmLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        NpmCoordinates coordinates = (NpmCoordinates) artifactCoordinates;

        String sb = (coordinates.getScope() != null ? "\"" + coordinates.getScope() + "/" : "\"") +
                    "" + coordinates.getName() + "\" : " +
                    "\"" + coordinates.getVersion() + "\"\n";

        // TODO: Add support for scopes

        return sb;
    }

}
