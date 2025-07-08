package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;

public interface ArtifactCoordinatesStrategy {

    ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity);

}
