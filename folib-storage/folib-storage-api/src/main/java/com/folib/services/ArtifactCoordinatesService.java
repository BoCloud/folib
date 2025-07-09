package com.folib.services;

import com.folib.artifact.coordinates.GenericCoordinates;

public interface ArtifactCoordinatesService {
    GenericCoordinates findById(String uuid);
}
