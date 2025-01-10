package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum ProductTypeEnum {

    /**
     * Maven
     */
    Maven(1, "maven", "Maven 2", "maven"),
    /**
     * Ivy
     */
    Ivy(2, "maven", "Maven 2", "ivy"),
    /**
     * SBT
     */
    SBT(3, "maven", "Maven 2", "sbt"),
    /**
     * Gradle
     */
    Gradle(4, "maven", "Maven 2", "gradle"),
    /**
     * Docker
     */
    Docker(5, "docker", "Docker", "docker"),
    /**
     * Helm
     */
    Helm(6, "helm", "helm", "helm"),
    /**
     * Pypi
     */
    Pypi(7, "pypi", "PyPi", "pypi"),
    /**
     * Npm
     */
    Npm(8, "npm", "npm", "npm"),
    /**
     * Yarn
     */
    Yarn(9, "npm", "npm", "yarn"),
    /**
     * ohpm
     */
    Ohpm(10, "npm", "npm", "ohpm"),
    /**
     * artifactory中是generic，nexus中是raw
     */
    Raw(11, "generic", "Raw", "raw"),
    /**
     * CocoaPods
     */
    CocoaPods(12, "cocoaPods", "cocoaPods", "cocoaPods"),
    /**
     * Go
     */
    Go(13, "go", "go", "go"),
    /**
     * Php
     */
    Php(14, "php", "php", "php"),
    /**
     * Conan
     */
    Conan(15, "conan", "conan", "conan"),
    /**
     * NuGet
     */
    NuGet(16, "nuget", "NuGet", "nuget"),
    /**
     * Rpm
     */
    Rpm(17, "rpm", "rpm", "rpm"),
    /**
     * GitLFS
     */
    GitLFS(18, "gitlfs", "gitlfs", "gitlfs"),
    /**
     * HuggingFace
     */
    HuggingFace(19, "HuggingFace", "HuggingFace", "HuggingFace"),
    /**
     * Pub
     */
    Pub(20, "pub", "pub", "pub"),
    /**
     * Debian
     */
    Debian(21, "debian", "debian", "debian"),
    /**
     * Cargo
     */
    Cargo(22, "cargo", "cargo", "cargo");;

    private Integer value;
    private String name;
    private String foLibraryName;
    private String subLayout;

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

}
