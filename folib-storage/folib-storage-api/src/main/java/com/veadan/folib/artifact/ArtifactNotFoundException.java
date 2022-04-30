package com.veadan.folib.artifact;

import java.io.FileNotFoundException;
import java.net.URI;

public class ArtifactNotFoundException extends FileNotFoundException
{

    private URI artifactResource;

    public ArtifactNotFoundException(URI artifactResource, String message)
    {
        super(message);
        
        this.artifactResource = artifactResource;
    }
    
    public ArtifactNotFoundException(URI artifactResource)
    {
        this(artifactResource, String.format("Artifact [%s] not found.", artifactResource));
    }

    public URI getArtifactResource()
    {
        return artifactResource;
    }

}
