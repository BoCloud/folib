package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PubArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("pubArtifactCoordinatesStrategy")
public class PubArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {

        PubArtifactCoordinates pubArtifactCoordinates = PubArtifactCoordinates.parse(entity.getPath());
        pubArtifactCoordinates.setUuid(entity.getUuid());
        pubArtifactCoordinates.setVersion(entity.getVersion());
        pubArtifactCoordinates.setNativeId(entity.getNativeId());
        pubArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(pubArtifactCoordinates);
        return pubArtifactCoordinates;
    }
}
