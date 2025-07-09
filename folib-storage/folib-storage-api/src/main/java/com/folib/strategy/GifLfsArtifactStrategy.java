package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GitLfsCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Component;

@Component("gifLfsArtifactStrategy")
public class GifLfsArtifactStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        GitLfsCoordinates gitLfsArtifactCoordinates = new GitLfsCoordinates(entity.getPath());
        gitLfsArtifactCoordinates.setUuid(entity.getUuid());
        gitLfsArtifactCoordinates.setVersion(entity.getVersion());
        gitLfsArtifactCoordinates.setNativeId(entity.getNativeId());
        gitLfsArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(gitLfsArtifactCoordinates);
        return gitLfsArtifactCoordinates;
    }
}
