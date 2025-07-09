package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.P2Coordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("p2CoordinatesStrategy")
public class P2CoordinatesStrategy implements ArtifactStrategy {


    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {

        P2Coordinates p2ArtifactCoordinates = P2Coordinates.create(entity.getPath());
        p2ArtifactCoordinates.setUuid(entity.getUuid());
        p2ArtifactCoordinates.setNativeId(entity.getNativeId());
        p2ArtifactCoordinates.setVersion(entity.getVersion());
        p2ArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(p2ArtifactCoordinates);
        return p2ArtifactCoordinates;
    }
}
