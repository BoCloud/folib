package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.HelmArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
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
        helmArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(helmArtifactCoordinates);
        return helmArtifactCoordinates;
    }
}
