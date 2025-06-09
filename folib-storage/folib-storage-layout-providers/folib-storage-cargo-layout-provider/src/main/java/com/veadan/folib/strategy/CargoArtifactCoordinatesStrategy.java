package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.CargoArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("cargoArtifactCoordinatesStrategy")
public class CargoArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        CargoArtifactCoordinates coordinates = new CargoArtifactCoordinates(entity.getPath());
        coordinates.setUuid(entity.getUuid());
        coordinates.setNativeId(entity.getNativeId());
        coordinates.setHierarchyParent(entity);
        coordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(coordinates);
        return coordinates;
    }
}
