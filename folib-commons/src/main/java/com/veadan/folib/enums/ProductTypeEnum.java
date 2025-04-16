package com.veadan.folib.enums;

import com.google.common.collect.Lists;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@AllArgsConstructor
@Getter
public enum ProductTypeEnum {

    /**
     * Maven
     */
    Maven(1, "maven", "Maven 2", "maven", "MavenArtifactCoordinates"),
    /**
     * Ivy
     */
    Ivy(2, "maven", "Maven 2", "ivy", "MavenArtifactCoordinates"),
    /**
     * SBT
     */
    SBT(3, "maven", "Maven 2", "sbt", "MavenArtifactCoordinates"),
    /**
     * Gradle
     */
    Gradle(4, "maven", "Maven 2", "gradle", "MavenArtifactCoordinates"),
    /**
     * Docker
     */
    Docker(5, "docker", "Docker", "docker", "DockerArtifactCoordinates"),
    /**
     * Helm
     */
    Helm(6, "helm", "helm", "helm", "HelmArtifactCoordinates"),
    /**
     * Pypi
     */
    Pypi(7, "pypi", "PyPi", "pypi", "PypiArtifactCoordinates"),
    /**
     * Npm
     */
    Npm(8, "npm", "npm", "npm", "NpmArtifactCoordinates"),
    /**
     * Yarn
     */
    Yarn(9, "npm", "npm", "yarn", "NpmArtifactCoordinates"),
    /**
     * ohpm
     */
    Ohpm(10, "npm", "npm", "ohpm", "NpmArtifactCoordinates"),
    /**
     * artifactory中是generic，nexus中是raw
     */
    Raw(11, "generic", "Raw", "raw", "RawArtifactCoordinates"),
    /**
     * CocoaPods
     */
    CocoaPods(12, "cocoaPods", "cocoapods", "cocoaPods", "CocoapodsArtifactCoordinates"),
    /**
     * Go
     */
    Go(13, "go", "go", "go", "GoArtifactCoordinates"),
    /**
     * Php
     */
    Php(14, "php", "php", "php", "PhpArtifactCoordinates"),
    /**
     * Conan
     */
    Conan(15, "conan", "conan", "conan", "ConanArtifactCoordinates"),
    /**
     * NuGet
     */
    NuGet(16, "nuget", "NuGet", "nuget", "NugetArtifactCoordinates"),
    /**
     * Rpm
     */
    Rpm(17, "rpm", "rpm", "rpm", "RpmArtifactCoordinates"),
    /**
     * GitLFS
     */
    GitLFS(18, "gitlfs", "GitLfs", "gitlfs", "GitLfsArtifactCoordinates"),
    /**
     * HuggingFace
     */
    HuggingFace(19, "HuggingFace", "HuggingFace", "huggingface", "HuggingFaceArtifactCoordinates"),
    /**
     * Pub
     */
    Pub(20, "pub", "pub", "pub", "PubArtifactCoordinates"),
    /**
     * Debian
     */
    Debian(21, "debian", "debian", "debian", "DebianArtifactCoordinates"),
    /**
     * Cargo
     */
    Cargo(22, "cargo", "cargo", "cargo", "CargoArtifactCoordinates"),
    /**
     * Conda
     */
    Conda(23, "conda", "conda", "conda", "CondaArtifactCoordinates");

    private Integer value;
    private String name;
    private String foLibraryName;
    private String subLayout;
    private String artifactCoordinates;

    public static List<String> SIMPLE_TYPE_LIST = Lists.newArrayList(Maven.getFoLibraryName(), Raw.getFoLibraryName(), Rpm.getFoLibraryName(), Debian.getFoLibraryName());

    public static String queryFolibLibraryByName(String name) {
        String libraryName = "";
        for (ProductTypeEnum productTypeEnum : ProductTypeEnum.values()) {
            if (productTypeEnum.getName().equalsIgnoreCase(name)) {

                libraryName = productTypeEnum.getFoLibraryName();
                break;
            }
        }
        return libraryName;
    }

    public static Integer queryValueByFoLibraryName(String foLibraryName) {
        Integer v = 1;
        for (ProductTypeEnum productTypeEnum : ProductTypeEnum.values()) {
            if (productTypeEnum.getFoLibraryName().equalsIgnoreCase(foLibraryName)) {
                v = productTypeEnum.getValue();
                break;
            }
        }
        return v;
    }

    public static String queryNameByFoLibraryName(String foLibraryName) {
        String name = "";
        for (ProductTypeEnum productTypeEnum : ProductTypeEnum.values()) {
            if (productTypeEnum.getFoLibraryName().equalsIgnoreCase(foLibraryName)) {
                name = productTypeEnum.getName();
                break;
            }
        }
        return name;
    }

    public static String queryFoLibraryNameByValue(Integer value) {
        String foLibraryName = "";
        for (ProductTypeEnum productTypeEnum : ProductTypeEnum.values()) {
            if (productTypeEnum.getValue().equals(value)) {
                foLibraryName = productTypeEnum.getFoLibraryName();
                break;
            }
        }
        return foLibraryName;
    }

    public static String queryArtifactCoordinatesByFoLibraryName(String foLibraryName) {
        String artifactCoordinates = "";
        for (ProductTypeEnum productTypeEnum : ProductTypeEnum.values()) {
            if (productTypeEnum.getFoLibraryName().equals(foLibraryName)) {
                artifactCoordinates = productTypeEnum.getArtifactCoordinates();
                break;
            }
        }
        return artifactCoordinates;
    }

}
