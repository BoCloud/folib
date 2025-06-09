package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Service("mavenArtifactCoordinatesStrategy")
public class MavenArtifactCoordinatesStrategyImpl implements ArtifactCoordinatesStrategy{

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {
        MavenArtifactCoordinates mavenArtifactCoordinates = new MavenArtifactCoordinates();
        mavenArtifactCoordinates.setNativeId(entity.getNativeId());
        mavenArtifactCoordinates.setVersion(entity.getVersion());
        mavenArtifactCoordinates.setGroupId(entity.getCoordinates().get("groupId"));
        mavenArtifactCoordinates.setUuid(entity.getUuid());
        mavenArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(mavenArtifactCoordinates);
        return mavenArtifactCoordinates;
    }
}
