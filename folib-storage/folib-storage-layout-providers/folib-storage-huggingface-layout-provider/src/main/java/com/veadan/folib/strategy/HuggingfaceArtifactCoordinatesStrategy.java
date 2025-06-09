package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("huggingFaceArtifactCoordinatesStrategy")
public class HuggingfaceArtifactCoordinatesStrategy implements  ArtifactCoordinatesStrategy{

    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        HuggingFaceArtifactCoordinates huggingFaceArtifactCoordinates = new HuggingFaceArtifactCoordinates(entity.getPath());
        huggingFaceArtifactCoordinates.setUuid(entity.getUuid());
        huggingFaceArtifactCoordinates.setVersion(entity.getVersion());
        huggingFaceArtifactCoordinates.setNativeId(entity.getNativeId());
        huggingFaceArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        entity.setHierarchyChild(huggingFaceArtifactCoordinates);
        return huggingFaceArtifactCoordinates;
    }
}
