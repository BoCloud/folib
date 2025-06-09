package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.P2ArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("p2ArtifactCoordinatesStrategy")
public class P2ArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{


    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {

        P2ArtifactCoordinates p2ArtifactCoordinates = P2ArtifactCoordinates.create(entity.getPath());
        p2ArtifactCoordinates.setUuid(entity.getUuid());
        p2ArtifactCoordinates.setNativeId(entity.getNativeId());
        p2ArtifactCoordinates.setVersion(entity.getVersion());
        p2ArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        entity.setHierarchyChild(p2ArtifactCoordinates);
        return p2ArtifactCoordinates;
    }
}
