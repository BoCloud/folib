package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.CargoCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("cargoCoordinatesStrategy")
public class CargoCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        CargoCoordinates coordinates = new CargoCoordinates(entity.getPath());
        coordinates.setUuid(entity.getUuid());
        coordinates.setNativeId(entity.getNativeId());
        coordinates.setHierarchyParent(entity);
        coordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(coordinates);
        return coordinates;
    }
}
