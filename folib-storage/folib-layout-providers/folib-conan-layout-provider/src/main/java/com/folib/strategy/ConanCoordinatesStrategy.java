package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.ConanCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("conanCoordinatesStrategy")
public class ConanCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity)
    {
        ConanCoordinates conanArtifactCoordinates = ConanCoordinates.parse(entity.getPath());
        conanArtifactCoordinates.setUuid(entity.getUuid());
        conanArtifactCoordinates.setVersion(entity.getVersion());
        conanArtifactCoordinates.setNativeId(entity.getNativeId());
        conanArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(conanArtifactCoordinates);
        return conanArtifactCoordinates;
    }
}
