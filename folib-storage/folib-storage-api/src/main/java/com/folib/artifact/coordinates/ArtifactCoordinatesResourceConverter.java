package com.folib.artifact.coordinates;


import com.folib.util.UriUtils;

import java.net.URI;


public interface ArtifactCoordinatesResourceConverter<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
{

    String convertToPath(C artifactCoordinates);

    default URI convertToResource(C artifactCoordinates)
    {
       try {
           return URI.create(UriUtils.encode(convertToPath(artifactCoordinates)));
       } catch (Exception ex) {
           ex.printStackTrace();
           throw new RuntimeException(ex.getMessage());
       }
    }

}
