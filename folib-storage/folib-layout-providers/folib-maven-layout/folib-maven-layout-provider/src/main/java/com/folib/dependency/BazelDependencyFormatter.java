package com.folib.dependency;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.dependency.snippet.CompatibleDependencyFormatRegistry;
import com.folib.dependency.snippet.DependencySynonymFormatter;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.providers.layout.Maven2LayoutProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Declan-Y
 *
 */
@Component
public class BazelDependencyFormatter
        implements DependencySynonymFormatter
{
    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);
    
    public static final String ALIAS = "Bazel";
    
    @Inject
    private CompatibleDependencyFormatRegistry compatibleDependencyFormatRegistry;
    
    
    @PostConstruct
    @Override
    public void register()
    {
        compatibleDependencyFormatRegistry.addProviderImplementation(getLayout(), getFormatAlias(), this);
        
        logger.info("Initialized the Bazel dependency formatter.");
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
        MavenCoordinates coordinates = (MavenCoordinates) artifactCoordinates;
        
        return "maven_jar(" +"\n"+"    name ="+" "
        +"\""+coordinates.getArtifactId()+"\""+","+
        (coordinates.getArtifactId() != null && coordinates.getGroupId() != null && coordinates.getVersion() != null ? 
        "\n    artifact = " + "\""+
        coordinates.getGroupId()+":"+coordinates.getArtifactId()+":"+coordinates.getVersion()+"\""+",\n)\n": 
        "\n)\n");
    }

}
