package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("conanArtifactCoordinatesStrategy")
public class ConanArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity)
    {
        ConanArtifactCoordinates conanArtifactCoordinates = ConanArtifactCoordinates.parse(entity.getPath());
        conanArtifactCoordinates.setUuid(entity.getUuid());
        conanArtifactCoordinates.setVersion(entity.getVersion());
        conanArtifactCoordinates.setNativeId(entity.getNativeId());
        conanArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(conanArtifactCoordinates);
        return conanArtifactCoordinates;
    }
}
