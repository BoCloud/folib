package com.folib.providers.repository;

import lombok.Data;

import java.util.Collection;

@Data
public class RepositorySearchRequest
{

    private final String artifactId;
    private final Collection<String> coordinateValues;
    private Boolean notPublishEvent;
    private Boolean useArtifactName;

    public RepositorySearchRequest(String artifactId,
                                   Collection<String> coordinateValues)
    {
        super();
        this.artifactId = artifactId;
        this.coordinateValues = coordinateValues;
    }

    public RepositorySearchRequest(String artifactId,
                                   Boolean useArtifactName,
                                   Collection<String> coordinateValues)
    {
        super();
        this.artifactId = artifactId;
        this.useArtifactName = useArtifactName;
        this.coordinateValues = coordinateValues;
    }

    public String getArtifactId()
    {
        return artifactId;
    }

    public Collection<String> getCoordinateValues()
    {
        return coordinateValues;
    }

}
