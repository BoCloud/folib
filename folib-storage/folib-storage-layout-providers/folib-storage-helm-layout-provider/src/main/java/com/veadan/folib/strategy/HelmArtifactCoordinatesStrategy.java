package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.HelmArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("helmArtifactCoordinatesStrategy")
public class HelmArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        if (entity.getCoordinates().isEmpty()) {
            return null;
        }
        String relativizePath = entity.getPath();
        String packageName = entity.getCoordinates().get("packageName");
        HelmArtifactCoordinates helmArtifactCoordinates = HelmArtifactCoordinates.parse(relativizePath, packageName);

        helmArtifactCoordinates.setUuid(entity.getUuid());
        helmArtifactCoordinates.setNativeId(entity.getNativeId());
        helmArtifactCoordinates.setVersion(entity.getVersion());
        helmArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        entity.setHierarchyChild(helmArtifactCoordinates);
        return helmArtifactCoordinates;
    }
}
