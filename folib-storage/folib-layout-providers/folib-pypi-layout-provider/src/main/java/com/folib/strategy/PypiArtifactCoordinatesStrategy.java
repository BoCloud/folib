package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("pypiArtifactCoordinatesStrategy")
public class PypiArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        PypiArtifactCoordinates pypiArtifactCoordinates = PypiArtifactCoordinates.parse(entity.getPath());
        pypiArtifactCoordinates.setUuid(entity.getUuid());
        pypiArtifactCoordinates.setVersion(entity.getVersion());
        pypiArtifactCoordinates.setNativeId(entity.getNativeId());
        pypiArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(pypiArtifactCoordinates);
        return pypiArtifactCoordinates;
    }
}
