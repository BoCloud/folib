package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GoArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("goArtifactCoordinatesStrategy")
public class GoArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        GoArtifactCoordinates goArtifactCoordinates = null;
        if(!entity.getCoordinates().isEmpty()){
            String name = entity.getCoordinates().get(GoArtifactCoordinates.NAME);
            String extension = entity.getCoordinates().get(GoArtifactCoordinates.EXTENSION);
            String version = entity.getVersion();
            goArtifactCoordinates = new GoArtifactCoordinates(name,  extension, version);
            goArtifactCoordinates.setUuid(entity.getUuid());
            goArtifactCoordinates.setNativeId(entity.getNativeId());
            goArtifactCoordinates.setVersion(entity.getVersion());
            goArtifactCoordinates.setHierarchyParent(entity);
            entity.setHierarchyChild(goArtifactCoordinates);
        }
        return goArtifactCoordinates;
    }
}
