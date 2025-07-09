package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.domain.GenericCoordinatesEntity;

public interface ArtifactStrategy {

    ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity);

}
