package com.veadan.folib.enums;

import com.veadan.folib.artifact.coordinates.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品仓库类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactoryFolibRepositoryTypeEnum {

    /**
     * Maven
     */
    MAVEN("Maven", MavenArtifactCoordinates.LAYOUT_NAME),
    /**
     * Docker
     */
    DOCKER("Docker", DockerArtifactCoordinates.LAYOUT_NAME),
    /**
     * PyPi
     */
    PYPI("PyPi", PypiArtifactCoordinates.LAYOUT_NAME),
    /**
     * Npm
     */
    NPM("Npm", NpmArtifactCoordinates.LAYOUT_NAME),
    /**
     * Raw
     */
    GENERIC("Generic", RawArtifactCoordinates.LAYOUT_NAME),
    /**
     * conan
     */
    CONAN("Conan", ConanArtifactCoordinates.LAYOUT_NAME);

    /**
     * Artifactory名称
     */
    private String name;

    /**
     * Folib名称
     */
    private String foLibraryName;

    public static String queryNameByFoLibraryName(String foLibraryName) {
        String name = "";
        for (ArtifactoryFolibRepositoryTypeEnum itemEnum : ArtifactoryFolibRepositoryTypeEnum.values()) {
            if (itemEnum.getFoLibraryName().equalsIgnoreCase(foLibraryName)) {
                name = itemEnum.getName();
                break;
            }
        }
        return name;
    }

}
