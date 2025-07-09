package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("pypiCoordinatesStrategy")
public class PypiCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        PypiCoordinates pypiArtifactCoordinates = PypiCoordinates.parse(entity.getPath());
        pypiArtifactCoordinates.setUuid(entity.getUuid());
        pypiArtifactCoordinates.setVersion(entity.getVersion());
        pypiArtifactCoordinates.setNativeId(entity.getNativeId());
        pypiArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(pypiArtifactCoordinates);
        return pypiArtifactCoordinates;
    }
}
