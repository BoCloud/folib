package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.RawCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("rawArtifactStrategy")
public class RawArtifactStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        RawCoordinates rawArtifactCoordinates = new RawCoordinates(entity.getCoordinates().get("path"));
        rawArtifactCoordinates.setUuid(entity.getUuid());
        rawArtifactCoordinates.setNativeId(entity.getNativeId());
        rawArtifactCoordinates.setVersion(entity.getVersion());
        rawArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyParent(rawArtifactCoordinates);
        return rawArtifactCoordinates;
    }
}
