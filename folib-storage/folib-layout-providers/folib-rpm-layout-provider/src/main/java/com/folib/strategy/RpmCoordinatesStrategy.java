package com.folib.strategy;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.domain.GenericCoordinatesEntity;
import com.folib.domain.RpmPackageArch;
import com.folib.domain.RpmPackageType;
import org.springframework.stereotype.Component;

@Component("rpmCoordinatesStrategy")
public class RpmCoordinatesStrategy implements ArtifactStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericCoordinatesEntity entity) {

        if (entity == null || entity.getCoordinates() == null) return null;
        RpmPackageType type = RpmPackageType.valueOf(entity.getCoordinates().get(RpmCoordinates.PACKAGE_TYPE));
        RpmCoordinates rpmArtifactCoordinates = null;
        if (entity.getCoordinates().containsKey(RpmCoordinates.ARCHITECTURE)) {

            rpmArtifactCoordinates = new RpmCoordinates(
                    entity.getCoordinates().get(RpmCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmCoordinates.VERSION),
                    entity.getCoordinates().get(RpmCoordinates.RELEASE),
                    type,
                    RpmPackageArch.valueOf(entity.getCoordinates().get(RpmCoordinates.ARCHITECTURE)),
                    entity.getCoordinates().get(RpmCoordinates.NAME)
            );
        } else {
            rpmArtifactCoordinates = new RpmCoordinates(
                    entity.getCoordinates().get(RpmCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmCoordinates.VERSION),
                    entity.getCoordinates().get(RpmCoordinates.RELEASE),
                    entity.getCoordinates().get(RpmCoordinates.NAME),
                    RpmPackageType.valueOf(entity.getCoordinates().get(RpmCoordinates.PACKAGE_TYPE))
            );
        }
        rpmArtifactCoordinates.setUuid(entity.getUuid());
        rpmArtifactCoordinates.setNativeId(entity.getNativeId());
        rpmArtifactCoordinates.setVersion(entity.getVersion());
        rpmArtifactCoordinates.setHierarchyParent(entity);
        entity.setHierarchyChild(rpmArtifactCoordinates);

        return rpmArtifactCoordinates;
    }
}
