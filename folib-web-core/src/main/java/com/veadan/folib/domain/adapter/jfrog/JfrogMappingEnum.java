package com.veadan.folib.domain.adapter.jfrog;

import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.HelmArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.RawArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author huayanjun
 * @since 2024-12-29 21:00
 */

@AllArgsConstructor
@Getter


/**

 *
 public static final String SUB_LAYOUT_GRADLE = "gradle";
 public static final String SUB_LAYOUT_MAVEN = "maven";
 public static final String SUB_LAYOUT_SBT = "sbt";
 public static final String SUB_LAYOUT_IVY = "ivy";


 *         layoutMap.put("Swift", "");
 *         layoutMap.put("Terraform", "");
 *         layoutMap.put("TerraformBE", "");
 *         layoutMap.put("Alpine", "");
 *         layoutMap.put("Bower", "");
 *         layoutMap.put("Cargo", "");
 *         layoutMap.put("Chef", "");
 *         layoutMap.put("Conda", "");
 *         layoutMap.put("CRAN", "");
 *         layoutMap.put("OCI", "");
 *         layoutMap.put("Gems", "");
 *         layoutMap.put("Opkg", "");

 *         layoutMap.put("Puppet", "");
 *         layoutMap.put("Vagrant", "");
 *         layoutMap.put("Generic", RawArtifactCoordinates.LAYOUT_NAME);
 */
public enum JfrogMappingEnum {

    Maven("Maven", MavenArtifactCoordinates.LAYOUT_NAME, "maven"),
    /**
     * Ivy
     */
    Ivy("helm", MavenArtifactCoordinates.LAYOUT_NAME, "ivy"),
    /**
     * SBT
     */
    SBT("SBT", MavenArtifactCoordinates.LAYOUT_NAME, "sbt"),
    /**
     * Gradle
     */
    Gradle("Gradle", MavenArtifactCoordinates.LAYOUT_NAME, "gradle"),
    /**
     * Docker
     */
    Docker( "Docker", DockerArtifactCoordinates.LAYOUT_NAME, "docker"),
    /**
     * Helm
     */
    Helm("Helm", HelmArtifactCoordinates.LAYOUT_NAME, "helm"),
    /**
     * Pypi
     */
    Pypi("Pypi", PypiArtifactCoordinates.LAYOUT_NAME, "pypi"),
    /**
     * Npm
     */
    Npm("Npm", NpmArtifactCoordinates.LAYOUT_NAME, "npm"),
    /**
     * Yarn
     */
    Yarn( "Yarn", NpmArtifactCoordinates.LAYOUT_NAME, "yarn"),
    /**
     * ohpm
     */
    Ohpm("npm", "npm", "ohpm"),
    /**

     */
    Raw( "Generic", RawArtifactCoordinates.LAYOUT_NAME, "raw"),
    /**
     * CocoaPods
     */
    CocoaPods( "CocoaPods", CocoapodsArtifactCoordinates.LAYOUT_NAME, "cocoaPods"),
    /**
     * Go
     */
    Go( "Go", GoArtifactCoordinates.LAYOUT_NAME, "go"),
    /**
     * Php
     */
    Php( "Composer", PhpArtifactCoordinates.LAYOUT_NAME, "php"),
    /**
     * Conan
     */
    Conan( "Conan", ConanArtifactCoordinates.LAYOUT_NAME, "conan"),
    /**
     * NuGet
     */
    NuGet("NuGet", NugetArtifactCoordinates.LAYOUT_NAME, "nuget"),
    /**
     * Rpm
     */
    Rpm("Rpm", RpmArtifactCoordinates.LAYOUT_NAME, "rpm"),
    /**
     * GitLFS
     */
    GitLFS( "GitLfs", GitLfsArtifactCoordinates.LAYOUT_NAME, "gitlfs"),
    /**
     * HuggingFace
     */
    HuggingFace( "HuggingFaceML", HuggingFaceArtifactCoordinates.LAYOUT_NAME, "HuggingFace"),
    /**
     * Pub
     */
    Pub("Pub", PubArtifactCoordinates.LAYOUT_NAME, "pub"),
    /**
     * Debian
     */
    Debian( "Debian", DebianConstant.LAYOUT_NAME, "debian");

    private final String name;
    private final String layout;
    private final String subLayout;

    public static JfrogMappingEnum getEnumByJfrogName(String name){
        for (JfrogMappingEnum jfrogMappingEnum : values()) {
            if(jfrogMappingEnum.getName().equals(name)){
                return jfrogMappingEnum;
            }
        }
        return null;
    }

    public static JfrogMappingEnum getEnumBySubLayout(String subLayout){
        for (JfrogMappingEnum jfrogMappingEnum : values()) {
            if(jfrogMappingEnum.getSubLayout().equals(subLayout)){
                return jfrogMappingEnum;
            }
        }
        return null;
    }
}
