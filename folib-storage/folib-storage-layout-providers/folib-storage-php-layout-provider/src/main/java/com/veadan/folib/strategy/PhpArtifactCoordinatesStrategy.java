package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
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
