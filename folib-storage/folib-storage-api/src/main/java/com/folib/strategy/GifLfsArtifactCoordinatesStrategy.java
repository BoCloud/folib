package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("gifLfsArtifactCoordinatesStrategy")
public class GifLfsArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        GitLfsArtifactCoordinates gitLfsArtifactCoordinates = new GitLfsArtifactCoordinates(entity.getPath());
        gitLfsArtifactCoordinates.setUuid(entity.getUuid());
        gitLfsArtifactCoordinates.setVersion(entity.getVersion());
        gitLfsArtifactCoordinates.setNativeId(entity.getNativeId());
        gitLfsArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(gitLfsArtifactCoordinates);
        return gitLfsArtifactCoordinates;
    }
}
