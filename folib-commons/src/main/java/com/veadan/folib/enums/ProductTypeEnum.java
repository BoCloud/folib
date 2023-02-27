package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum ProductTypeEnum {

    Maven(1, "maven", "Maven 2"),
    Docker(2, "docker", "Docker"),
    Pypi(3, "pypi", "PyPi"),
    Npm(4, "npm", "npm"),
    /**
     * artifactory中是generic，nexus中是raw
     */
    Generic(5, "generic", "Raw"),
    Conan(6, "conan", "conan");

    private Integer value;
    private String name;
    private String foLibraryName;

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
