package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;

public interface ArtifactCoordinatesStrategy {

    ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity);

}
