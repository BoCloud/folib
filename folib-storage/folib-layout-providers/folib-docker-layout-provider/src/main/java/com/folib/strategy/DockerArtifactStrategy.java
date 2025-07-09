package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("dockerArtifactCoordinatesStrategy")
public class DockerArtifactStrategy implements ArtifactStrategy {


    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        DockerCoordinates dockerArtifactCoordinates = null;
        if (!entity.getCoordinates().isEmpty()) {
            String imageName = entity.getCoordinates().get(DockerCoordinates.IMAGE_NAME);
            String reference = entity.getVersion();
            String layers = entity.getCoordinates().get(DockerCoordinates.LAYERS);
            String artifactPath = entity.getCoordinates().get(DockerCoordinates.ARTIFACT_PATH);
            dockerArtifactCoordinates = new DockerCoordinates(imageName, reference, layers, artifactPath);
            dockerArtifactCoordinates.setUuid(entity.getUuid());
            dockerArtifactCoordinates.setNativeId(entity.getNativeId());
            dockerArtifactCoordinates.setHierarchyParent(entity);
            dockerArtifactCoordinates.setVersion(entity.getVersion());
            entity.setHierarchyParent(dockerArtifactCoordinates);
        }
        return dockerArtifactCoordinates;
    }
}
