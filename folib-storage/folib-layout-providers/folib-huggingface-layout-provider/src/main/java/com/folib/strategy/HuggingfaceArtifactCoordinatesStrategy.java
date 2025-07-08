package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("huggingFaceArtifactCoordinatesStrategy")
public class HuggingfaceArtifactCoordinatesStrategy implements  ArtifactCoordinatesStrategy{

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        HuggingFaceArtifactCoordinates huggingFaceArtifactCoordinates = new HuggingFaceArtifactCoordinates(entity.getPath());
        huggingFaceArtifactCoordinates.setUuid(entity.getUuid());
        huggingFaceArtifactCoordinates.setVersion(entity.getVersion());
        huggingFaceArtifactCoordinates.setNativeId(entity.getNativeId());
        huggingFaceArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(huggingFaceArtifactCoordinates);
        return huggingFaceArtifactCoordinates;
    }
}
