package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("nugetArtifactCoordinatesStrategy")
public class NugetArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {

        if (entity == null || entity.getCoordinates() == null) return null;
        NugetArtifactCoordinates nugetArtifactCoordinates = new NugetArtifactCoordinates(entity.getCoordinates().get(NugetArtifactCoordinates.ID),
                entity.getCoordinates().get(NugetArtifactCoordinates.VERSION),
                entity.getCoordinates().get(NugetArtifactCoordinates.EXTENSION));
        nugetArtifactCoordinates.setNativeId(entity.getNativeId());
        nugetArtifactCoordinates.setUuid(entity.getUuid());
        nugetArtifactCoordinates.setVersion(entity.getVersion());
        nugetArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyParent(nugetArtifactCoordinates);
        return nugetArtifactCoordinates;
    }
}
