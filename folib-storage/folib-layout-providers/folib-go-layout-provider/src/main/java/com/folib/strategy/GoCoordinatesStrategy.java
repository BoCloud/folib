package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GoCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("goCoordinatesStrategy")
public class GoCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        GoCoordinates goArtifactCoordinates = null;
        if(!entity.getCoordinates().isEmpty()){
            String name = entity.getCoordinates().get(GoCoordinates.NAME);
            String extension = entity.getCoordinates().get(GoCoordinates.EXTENSION);
            String version = entity.getVersion();
            goArtifactCoordinates = new GoCoordinates(name,  extension, version);
            goArtifactCoordinates.setUuid(entity.getUuid());
            goArtifactCoordinates.setNativeId(entity.getNativeId());
            goArtifactCoordinates.setVersion(entity.getVersion());
            goArtifactCoordinates.setHierarchyParent(entity);
            entity.setHierarchyChild(goArtifactCoordinates);
        }
        return goArtifactCoordinates;
    }
}
