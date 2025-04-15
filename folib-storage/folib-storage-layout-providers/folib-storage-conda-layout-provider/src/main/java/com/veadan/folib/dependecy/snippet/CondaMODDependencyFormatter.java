package com.veadan.folib.dependecy.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.veadan.folib.dependency.snippet.DependencySynonymFormatter;
import com.veadan.folib.providers.layout.CondaLayoutProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author LingengMa
 * @date 2025/04/02 14:12
 * @Description:
 */
@Component
public class CondaMODDependencyFormatter implements DependencySynonymFormatter {
    private static final Logger logger = LoggerFactory.getLogger(CondaMODDependencyFormatter.class);

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
        return CondaLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return CondaLayoutProvider.ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        //TODO
        return "";
    }
}
