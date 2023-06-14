package com.veadan.folib.services;

import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;

public interface ArtifactCoordinatesService {
    GenericArtifactCoordinates findById(String uuid);
}
