package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.DebianCoordinates;
import com.folib.constant.DebianConstant;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("debianArtifactStrategy")
public class DebianCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        DebianCoordinates debianArtifactCoordinates = null;
        if(!entity.getCoordinates().isEmpty()){
             debianArtifactCoordinates = new DebianCoordinates();
        }else {
            String component = entity.getCoordinates().get(DebianConstant.COMPONENT);
            String name = entity.getCoordinates().get(DebianConstant.NAME);
            String extension = entity.getCoordinates().get(DebianConstant.EXTENSION);
            debianArtifactCoordinates = new DebianCoordinates(component,  name, extension);
        }
        debianArtifactCoordinates.setUuid(entity.getUuid());
        debianArtifactCoordinates.setNativeId(entity.getNativeId());
        debianArtifactCoordinates.setHierarchyParent(entity);
        debianArtifactCoordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(debianArtifactCoordinates);
        return debianArtifactCoordinates;
    }
}
