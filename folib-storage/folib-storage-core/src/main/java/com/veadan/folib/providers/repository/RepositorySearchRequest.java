package com.veadan.folib.providers.repository;

import java.util.Collection;

public class RepositorySearchRequest
{

    private final String artifactId;
    private final Collection<String> coordinateValues;

    public RepositorySearchRequest(String artifactId,
                                   Collection<String> coordinateValues)
    {
        super();
        this.artifactId = artifactId;
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
