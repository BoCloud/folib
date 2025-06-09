package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("npmArtifactCoordinatesStrategy")
public class NpmArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy{

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        if(entity == null || entity.getCoordinates() == null){
          return null;
        }
        String scope = entity.getCoordinates().get(NpmArtifactCoordinates.SCOPE);
        String name =  entity.getCoordinates().get(NpmArtifactCoordinates.NAME);
        String version =  entity.getCoordinates().get(NpmArtifactCoordinates.VERSION);
        String extension =  entity.getCoordinates().get(NpmArtifactCoordinates.EXTENSION);
        String distribution =  entity.getCoordinates().get(NpmArtifactCoordinates.DISTRIBUTION);
        NpmArtifactCoordinates npmArtifactCoordinates = new NpmArtifactCoordinates(scope,  name, version, extension, distribution);
        npmArtifactCoordinates.setNativeId(entity.getNativeId());
        npmArtifactCoordinates.setUuid(entity.getUuid());
        npmArtifactCoordinates.setVersion(entity.getVersion());
        npmArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        entity.setHierarchyChild(npmArtifactCoordinates);
        return npmArtifactCoordinates;
    }
}
