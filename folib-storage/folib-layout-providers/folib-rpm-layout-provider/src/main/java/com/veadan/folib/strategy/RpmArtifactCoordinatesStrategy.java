package com.veadan.folib.strategy;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.domain.GenericArtifactCoordinatesEntity;
import com.veadan.folib.domain.RpmPackageArch;
import com.veadan.folib.domain.RpmPackageType;
import org.springframework.stereotype.Component;

@Component("rpmArtifactCoordinatesStrategy")
public class RpmArtifactCoordinatesStrategy implements ArtifactCoordinatesStrategy {

    @Override
    public ArtifactCoordinates getArtifactCoordinates(GenericArtifactCoordinatesEntity entity) {

        if (entity == null || entity.getCoordinates() == null) return null;
        RpmPackageType type = RpmPackageType.valueOf(entity.getCoordinates().get(RpmArtifactCoordinates.PACKAGE_TYPE));
        RpmArtifactCoordinates rpmArtifactCoordinates = null;
        if (entity.getCoordinates().containsKey(RpmArtifactCoordinates.ARCHITECTURE)) {

            rpmArtifactCoordinates = new RpmArtifactCoordinates(
                    entity.getCoordinates().get(RpmArtifactCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmArtifactCoordinates.VERSION),
                    entity.getCoordinates().get(RpmArtifactCoordinates.RELEASE),
                    type,
                    RpmPackageArch.valueOf(entity.getCoordinates().get(RpmArtifactCoordinates.ARCHITECTURE)),
                    entity.getCoordinates().get(RpmArtifactCoordinates.NAME)
            );
        } else {
            rpmArtifactCoordinates = new RpmArtifactCoordinates(
                    entity.getCoordinates().get(RpmArtifactCoordinates.BASE_NAME),
                    entity.getCoordinates().get(RpmArtifactCoordinates.VERSION),
                    entity.getCoordinates().get(RpmArtifactCoordinates.RELEASE),
                    entity.getCoordinates().get(RpmArtifactCoordinates.NAME),
                    RpmPackageType.valueOf(entity.getCoordinates().get(RpmArtifactCoordinates.PACKAGE_TYPE))
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
