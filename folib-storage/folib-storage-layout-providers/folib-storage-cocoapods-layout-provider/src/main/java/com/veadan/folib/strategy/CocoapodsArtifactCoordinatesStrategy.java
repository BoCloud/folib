package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("cocoapodsArtifactCoordinatesStrategy")
public class CocoapodsArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        CocoapodsArtifactCoordinates coordinates = new CocoapodsArtifactCoordinates(entity.getPath());
        coordinates.setUuid(entity.getUuid());
        coordinates.setNativeId(entity.getNativeId());
        coordinates.setHierarchyParent(entity);
        coordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(coordinates);
        return coordinates;
    }
}
