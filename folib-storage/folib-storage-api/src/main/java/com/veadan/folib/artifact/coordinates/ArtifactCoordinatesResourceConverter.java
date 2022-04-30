package com.veadan.folib.artifact.coordinates;

import java.net.URI;

public interface ArtifactCoordinatesResourceConverter<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
{

    String convertToPath(C artifactCoordinates);

    default URI convertToResource(C artifactCoordinates)
    {
        return URI.create(convertToPath(artifactCoordinates));
    }

}
