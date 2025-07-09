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
package com.folib.domain.adapter.jfrog;

import com.folib.artifact.coordinates.CocoapodsCoordinates;
import com.folib.artifact.coordinates.ConanCoordinates;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.artifact.coordinates.GitLfsCoordinates;
import com.folib.artifact.coordinates.GoCoordinates;
import com.folib.artifact.coordinates.HelmCoordinates;
import com.folib.artifact.coordinates.HuggingFaceCoordinates;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.artifact.coordinates.PubCoordinates;
import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.artifact.coordinates.RawCoordinates;
import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.constant.DebianConstant;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author veadan
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

    Maven("Maven", MavenCoordinates.LAYOUT_NAME, "maven"),
    /**
     * Ivy
     */
    Ivy("helm", MavenCoordinates.LAYOUT_NAME, "ivy"),
    /**
     * SBT
     */
    SBT("SBT", MavenCoordinates.LAYOUT_NAME, "sbt"),
    /**
     * Gradle
     */
    Gradle("Gradle", MavenCoordinates.LAYOUT_NAME, "gradle"),
    /**
     * Docker
     */
    Docker( "Docker", DockerCoordinates.LAYOUT_NAME, "docker"),
    /**
     * Helm
     */
    Helm("Helm", HelmCoordinates.LAYOUT_NAME, "helm"),
    /**
     * Pypi
     */
    Pypi("Pypi", PypiCoordinates.LAYOUT_NAME, "pypi"),
    /**
     * Npm
     */
    Npm("Npm", NpmCoordinates.LAYOUT_NAME, "npm"),
    /**
     * Yarn
     */
    Yarn( "Yarn", NpmCoordinates.LAYOUT_NAME, "yarn"),
    /**
     * ohpm
     */
    Ohpm("npm", "npm", "ohpm"),
    /**

     */
    Raw( "Generic", RawCoordinates.LAYOUT_NAME, "raw"),
    /**
     * CocoaPods
     */
    CocoaPods( "CocoaPods", CocoapodsCoordinates.LAYOUT_NAME, "cocoaPods"),
    /**
     * Go
     */
    Go( "Go", GoCoordinates.LAYOUT_NAME, "go"),
    /**
     * Php
     */
    Php( "Composer", PhpCoordinates.LAYOUT_NAME, "php"),
    /**
     * Conan
     */
    Conan( "Conan", ConanCoordinates.LAYOUT_NAME, "conan"),
    /**
     * NuGet
     */
    NuGet("NuGet", NugetCoordinates.LAYOUT_NAME, "nuget"),
    /**
     * Rpm
     */
    Rpm("Rpm", RpmCoordinates.LAYOUT_NAME, "rpm"),

    Yum("YUM", RpmCoordinates.LAYOUT_NAME,"rpm"),
    /**
     * GitLFS
     */
    GitLFS( "GitLfs", GitLfsCoordinates.LAYOUT_NAME, "gitlfs"),
    /**
     * HuggingFace
     */
    HuggingFace( "HuggingFaceML", HuggingFaceCoordinates.LAYOUT_NAME, "HuggingFace"),
    /**
     * Pub
     */
    Pub("Pub", PubCoordinates.LAYOUT_NAME, "pub"),
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
