package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("debianArtifactCoordinatesStrategy")
public class DebianArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        DebianArtifactCoordinates debianArtifactCoordinates = null;
        if(!entity.getCoordinates().isEmpty()){
             debianArtifactCoordinates = new DebianArtifactCoordinates();
        }else {
            String component = entity.getCoordinates().get(DebianConstant.COMPONENT);
            String name = entity.getCoordinates().get(DebianConstant.NAME);
            String extension = entity.getCoordinates().get(DebianConstant.EXTENSION);
            debianArtifactCoordinates = new DebianArtifactCoordinates(component,  name, extension);
        }
        debianArtifactCoordinates.setUuid(entity.getUuid());
        debianArtifactCoordinates.setNativeId(entity.getNativeId());
        debianArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        debianArtifactCoordinates.setVersion(entity.getVersion());
        entity.setHierarchyChild(debianArtifactCoordinates);
        return debianArtifactCoordinates;
    }
}
