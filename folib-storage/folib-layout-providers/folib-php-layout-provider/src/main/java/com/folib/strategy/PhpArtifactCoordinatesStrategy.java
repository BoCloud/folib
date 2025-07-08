package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("phpArtifactCoordinatesStrategy")
public class PhpArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        PhpArtifactCoordinates phpArtifactCoordinates = PhpArtifactCoordinates.parse(entity.getPath());
        phpArtifactCoordinates.setUuid(entity.getUuid());
        phpArtifactCoordinates.setVersion(entity.getVersion());
        phpArtifactCoordinates.setNativeId(entity.getNativeId());
        phpArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(phpArtifactCoordinates);
        return phpArtifactCoordinates;
    }
}
