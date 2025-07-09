package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.CocoapodsCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("cocoapodsCoordinatesStrategy")
public class CocoapodsCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        CocoapodsCoordinates coordinates = new CocoapodsCoordinates(entity.getPath());
        coordinates.setUuid(entity.getUuid());
        coordinates.setNativeId(entity.getNativeId());
        coordinates.setHierarchyParent(entity);
        coordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(coordinates);
        return coordinates;
    }
}
