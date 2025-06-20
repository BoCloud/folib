package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("dockerArtifactCoordinatesStrategy")
public class DockerArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {


    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        DockerArtifactCoordinates dockerArtifactCoordinates = null;
        if (!entity.getCoordinates().isEmpty()) {
            String imageName = entity.getCoordinates().get(DockerArtifactCoordinates.IMAGE_NAME);
            String reference = entity.getVersion();
            String layers = entity.getCoordinates().get(DockerArtifactCoordinates.LAYERS);
            String artifactPath = entity.getCoordinates().get(DockerArtifactCoordinates.ARTIFACT_PATH);
            dockerArtifactCoordinates = new DockerArtifactCoordinates(imageName, reference, layers, artifactPath);
            dockerArtifactCoordinates.setUuid(entity.getUuid());
            dockerArtifactCoordinates.setNativeId(entity.getNativeId());
            dockerArtifactCoordinates.setHierarchyParent(entity);
            dockerArtifactCoordinates.setVersion(entity.getVersion());
            entity.setHierarchyParent(dockerArtifactCoordinates);
        }
        return dockerArtifactCoordinates;
    }
}
