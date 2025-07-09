package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("npmArtifactCoordinatesStrategy")
public class NpmCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        if(entity == null || entity.getCoordinates() == null){
          return null;
        }
        String scope = entity.getCoordinates().get(NpmCoordinates.SCOPE);
        String name =  entity.getCoordinates().get(NpmCoordinates.NAME);
        String version =  entity.getCoordinates().get(NpmCoordinates.VERSION);
        String extension =  entity.getCoordinates().get(NpmCoordinates.EXTENSION);
        String distribution =  entity.getCoordinates().get(NpmCoordinates.DISTRIBUTION);
        NpmCoordinates npmArtifactCoordinates = new NpmCoordinates(scope,  name, version, extension, distribution);
        npmArtifactCoordinates.setNativeId(entity.getNativeId());
        npmArtifactCoordinates.setUuid(entity.getUuid());
        npmArtifactCoordinates.setVersion(entity.getVersion());
        npmArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(npmArtifactCoordinates);
        return npmArtifactCoordinates;
    }
}
