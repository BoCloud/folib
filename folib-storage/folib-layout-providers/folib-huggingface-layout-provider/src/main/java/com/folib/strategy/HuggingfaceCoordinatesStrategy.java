package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.HuggingFaceCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("huggingFaceCoordinatesStrategy")
public class HuggingfaceCoordinatesStrategy implements ArtifactStrategy {

    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        HuggingFaceCoordinates huggingFaceArtifactCoordinates = new HuggingFaceCoordinates(entity.getPath());
        huggingFaceArtifactCoordinates.setUuid(entity.getUuid());
        huggingFaceArtifactCoordinates.setVersion(entity.getVersion());
        huggingFaceArtifactCoordinates.setNativeId(entity.getNativeId());
        huggingFaceArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(huggingFaceArtifactCoordinates);
        return huggingFaceArtifactCoordinates;
    }
}
