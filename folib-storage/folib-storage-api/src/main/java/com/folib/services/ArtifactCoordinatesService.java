package com.folib.services;

import com.folib.artifact.coordinates.GenericArtifactCoordinates;

public interface ArtifactCoordinatesService {
    GenericArtifactCoordinates findById(String uuid);
}
