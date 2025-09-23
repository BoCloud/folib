package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.CondaCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("condaCoordinatesStrategy")
public class CondaCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        CondaCoordinates conanArtifactCoordinates = CondaCoordinates.parse(entity.getPath());
        conanArtifactCoordinates.setUuid(entity.getUuid());
        conanArtifactCoordinates.setVersion(entity.getVersion());
        conanArtifactCoordinates.setNativeId(entity.getNativeId());
        conanArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(conanArtifactCoordinates);
        return conanArtifactCoordinates;
    }
}
