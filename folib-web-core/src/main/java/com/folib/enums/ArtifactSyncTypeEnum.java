/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品同步
 *
 * @author veadan
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
