package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.RawArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("rawArtifactCoordinatesStrategy")
public class RawArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        RawArtifactCoordinates rawArtifactCoordinates = new RawArtifactCoordinates(entity.getCoordinates().get("path"));
        rawArtifactCoordinates.setUuid(entity.getUuid());
        rawArtifactCoordinates.setNativeId(entity.getNativeId());
        rawArtifactCoordinates.setVersion(entity.getVersion());
        rawArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyParent(rawArtifactCoordinates);
        return rawArtifactCoordinates;
    }
}
