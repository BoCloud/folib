package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("phpCoordinatesStrategy")
public class PhpCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        PhpCoordinates phpArtifactCoordinates = PhpCoordinates.parse(entity.getPath());
        phpArtifactCoordinates.setUuid(entity.getUuid());
        phpArtifactCoordinates.setVersion(entity.getVersion());
        phpArtifactCoordinates.setNativeId(entity.getNativeId());
        phpArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(phpArtifactCoordinates);
        return phpArtifactCoordinates;
    }
}
