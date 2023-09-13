package com.veadan.folib.dependency.snippet;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.providers.layout.CocoapodsLayoutProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/9/6 23:58
 * @since x.x.x
 */
@Component
public class CocoapodsDependencyFormatter
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

        logger.info("Initialized the Cocoapods dependency formatter.");
    }

    @Override
    public String getLayout()
    {
        return CocoapodsLayoutProvider.ALIAS;
    }

    @Override
    public String getFormatAlias()
    {
        return CocoapodsLayoutProvider.ALIAS;
    }

    @Override
    public String getDependencySnippet(ArtifactCoordinates artifactCoordinates)
    {
        CocoapodsArtifactCoordinates coordinates = (CocoapodsArtifactCoordinates) artifactCoordinates;
        String sb = "target 'MyApp' do\n" +
                "  pod '"+coordinates.getBaseName()+"', '~> "+coordinates.getVersion()+"'\n" +
                "end";

        return sb;
    }

}
