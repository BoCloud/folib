package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.HelmCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("helmCoordinatesStrategy")
public class HelmCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        if (entity.getCoordinates().isEmpty()) {
            return null;
        }
        String relativizePath = entity.getPath();
        String packageName = entity.getCoordinates().get("packageName");
        HelmCoordinates helmArtifactCoordinates = HelmCoordinates.parse(relativizePath, packageName);

        helmArtifactCoordinates.setUuid(entity.getUuid());
        helmArtifactCoordinates.setNativeId(entity.getNativeId());
        helmArtifactCoordinates.setVersion(entity.getVersion());
        helmArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(helmArtifactCoordinates);
        return helmArtifactCoordinates;
    }
}
