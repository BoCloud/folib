package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import org.springframework.stereotype.Service;

@Service("mavenCoordinatesStrategy")
public class MavenArtifactCoordinatesStrategyImpl implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {
        MavenCoordinates mavenArtifactCoordinates = new MavenCoordinates();
        mavenArtifactCoordinates.setNativeId(entity.getNativeId());
        mavenArtifactCoordinates.setVersion(entity.getVersion());
        mavenArtifactCoordinates.setGroupId(entity.getCoordinates().get("groupId"));
        mavenArtifactCoordinates.setUuid(entity.getUuid());
        mavenArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(mavenArtifactCoordinates);
        return mavenArtifactCoordinates;
    }
}
