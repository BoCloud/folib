package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("gifLfsArtifactCoordinatesStrategy")
public class GifLfsArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        GitLfsArtifactCoordinates gitLfsArtifactCoordinates = new GitLfsArtifactCoordinates(entity.getPath());
        gitLfsArtifactCoordinates.setUuid(entity.getUuid());
        gitLfsArtifactCoordinates.setVersion(entity.getVersion());
        gitLfsArtifactCoordinates.setNativeId(entity.getNativeId());
        gitLfsArtifactCoordinates.setHierarchyParent(entity.getHierarchyParent());
        entity.setHierarchyChild(gitLfsArtifactCoordinates);
        return gitLfsArtifactCoordinates;
    }
}
