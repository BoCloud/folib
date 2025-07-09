package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("nugetCoordinatesStrategy")
public class NugetArtifactCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {

        if (entity == null || entity.getCoordinates() == null) return null;
        NugetCoordinates nugetArtifactCoordinates = new NugetCoordinates(entity.getCoordinates().get(NugetCoordinates.ID),
                entity.getCoordinates().get(NugetCoordinates.VERSION),
                entity.getCoordinates().get(NugetCoordinates.EXTENSION));
        nugetArtifactCoordinates.setNativeId(entity.getNativeId());
        nugetArtifactCoordinates.setUuid(entity.getUuid());
        nugetArtifactCoordinates.setVersion(entity.getVersion());
        nugetArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyParent(nugetArtifactCoordinates);
        return nugetArtifactCoordinates;
    }
}
