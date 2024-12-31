package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品同步
 *
 * @author leipenghui
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactSyncTypeEnum {

    /**
     * RAW
     */
    RAW("SYNC_RAW", ProductTypeEnum.Raw.getFoLibraryName()),
    /**
     * MAVEN
     */
    MAVEN("SYNC_MAVEN", ProductTypeEnum.Maven.getFoLibraryName()),
    /**
     * HELM
     */
    NPM("SYNC_NPM", ProductTypeEnum.Npm.getFoLibraryName()),
    /**
     * HELM
     */
    HELM("SYNC_HELM", ProductTypeEnum.Helm.getFoLibraryName()),
    /**
     * RPM
     */
    RPM("SYNC_RPM", ProductTypeEnum.Rpm.getFoLibraryName()),
    /**
     * PYPI
     */
    PYPI("SYNC_PYPI", ProductTypeEnum.Pypi.getFoLibraryName()),
    /**
     * DEBIAN
     */
    DEBIAN("SYNC_DEBIAN", ProductTypeEnum.Debian.getFoLibraryName()),


    DOCKER("SYNC_DOCKER", ProductTypeEnum.Docker.getFoLibraryName()),

    GO("SYNC_GO",ProductTypeEnum.Go.getFoLibraryName())
    ;

    /**
     * type
     */
    private String type;

    /**
     * repositoryType
     */
    private String repositoryType;

    public static String resolveType(String repositoryType) {
        String type = "";
        for (ArtifactSyncTypeEnum item : ArtifactSyncTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }
}
