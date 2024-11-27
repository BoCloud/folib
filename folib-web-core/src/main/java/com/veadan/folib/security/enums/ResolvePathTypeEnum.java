package com.veadan.folib.security.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 条件类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ResolvePathTypeEnum {

    /**
     * docker
     */
    DOCKER("dockerResolvePath"),
    /**
     * npm
     */
    NPM("npmResolvePath"),
    /**
     * pypi
     */
    PYPI("pypiResolvePath"),
    /**
     * pub
     */
    PUB("pubResolvePath"),
    /**
     * conan
     */
    CONAN("conanResolvePath"),
    /**
     * helm
     */
    HELM("helmResolvePath"),
    ;

    /**
     * type
     */
    private String type;

    public static String getResolvePathType(String layout) {
        String type = "";
        for (ResolvePathTypeEnum resolvePathTypeEnum : ResolvePathTypeEnum.values()) {
            if (resolvePathTypeEnum.toString().equalsIgnoreCase(layout)) {
                type = resolvePathTypeEnum.type;
                break;
            }
        }
        return type;
    }

}
