package com.folib.index.indexer;

import com.folib.index.model.Index;
import com.folib.index.model.RepoDataPackage;
import lombok.Generated;

public class IndexToRepodataPackageAdapter {
    public static RepoDataPackage adapt(Index index) {
        RepoDataPackage repoDataPackage = new RepoDataPackage();
        repoDataPackage.setBuild(index.getBuild());
        repoDataPackage.setBuildNumber(index.getBuildNumber());
        repoDataPackage.setConstrains(index.getConstrains());
        repoDataPackage.setDepends(index.getDepends());
        repoDataPackage.setLicense(index.getLicense());
        repoDataPackage.setLicenseFamily(index.getLicenseFamily());
        repoDataPackage.setName(index.getName());
        repoDataPackage.setVersion(index.getVersion());
        repoDataPackage.setSubdir(index.getSubdir());
        repoDataPackage.setTimestamp(index.getTimestamp());
        repoDataPackage.setNoarch(index.getNoarch());
        repoDataPackage.setFeatures(index.getFeatures());
        repoDataPackage.setTrackFeatures(index.getTrackFeatures());
        repoDataPackage.setIndex(index);
        return repoDataPackage;
    }

    @Generated
    private IndexToRepodataPackageAdapter() {
    }
}
