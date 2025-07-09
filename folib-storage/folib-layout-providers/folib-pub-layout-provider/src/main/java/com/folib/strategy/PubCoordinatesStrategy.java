package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PubCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("pubCoordinatesStrategy")
public class PubCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {

        PubCoordinates pubArtifactCoordinates = PubCoordinates.parse(entity.getPath());
        pubArtifactCoordinates.setUuid(entity.getUuid());
        pubArtifactCoordinates.setVersion(entity.getVersion());
        pubArtifactCoordinates.setNativeId(entity.getNativeId());
        pubArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(pubArtifactCoordinates);
        return pubArtifactCoordinates;
    }
}
