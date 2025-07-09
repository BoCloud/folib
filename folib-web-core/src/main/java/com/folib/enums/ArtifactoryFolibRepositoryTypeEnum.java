package com.folib.enums;

import com.folib.artifact.coordinates.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品仓库类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactoryFolibRepositoryTypeEnum {

    /**
     * Maven
     */
    MAVEN("Maven", MavenCoordinates.LAYOUT_NAME),
    /**
     * Docker
     */
    DOCKER("Docker", DockerCoordinates.LAYOUT_NAME),
    /**
     * PyPi
     */
    PYPI("PyPi", PypiCoordinates.LAYOUT_NAME),
    /**
     * Npm
     */
    NPM("Npm", NpmCoordinates.LAYOUT_NAME),
    /**
     * Raw
     */
    GENERIC("Generic", RawCoordinates.LAYOUT_NAME),
    /**
     * conan
     */
    CONAN("Conan", ConanCoordinates.LAYOUT_NAME);

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
