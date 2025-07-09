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
package com.folib.artifact.coordinates;


import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.neo4j.ogm.annotation.NodeEntity;


import java.net.URI;

@NodeEntity(Vertices.HELM_COORDINATES)
@XmlRootElement(name = "HelmCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = "helm", alias = "helm")
@Slf4j
@Data
public class HelmCoordinates extends LayoutCoordinatesEntity<HelmCoordinates, ComparableVersion> {
    public static String LAYOUT_NAME = "helm";
    private String LAYOUT_ALIAS = "helm";
    private String BASE_NAME = "base_name";
    private String PACKAGE_TYPE = "Chart";
    private String META_DATA = "false";// index.yaml
    private String ARTIFACT_SUFFIX = ".tgz";// .tgz.prov
    private static final String VERSION = "version";
    private static final String PATH = "path";
    private static final String NAME = "name";
    public static final String DESCRIPTION = "description";
    private static final String EXTENSION = "extension";

    public HelmCoordinates() {
        resetCoordinates(NAME);
    }

    public HelmCoordinates(String relativizePath, String packageName) {

        // 正则表达式匹配 Helm 包名
//        String regex = "^(?<name>[a-z0-9-]+)-(\\d+(?:\\.\\d+)+(?:-[a-zA-Z0-9]+)?)\\.tgz$";
//        Pattern pattern = Pattern.compile(regex);
//        Matcher matcher = pattern.matcher(packageName);
        setId(relativizePath);
//        if (matcher.find()) {
//            String name = matcher.group("name");
//            String version = matcher.group(2);
//            setVersion(version);
//        } else {
//           throw new RuntimeException("Invalid Helm package name format.");
//        }

    }

    public static HelmCoordinates parse(String relativizePath, String packageName) {
        log.info("parse helm relativizePath {}", relativizePath);
        return new HelmCoordinates(relativizePath,packageName);
    }

    @Override
    public String getId() {
        return getName();
    }

    public void setId(String id) {
        setCoordinate(NAME, id);
    }

    public void setVersion(String version) {
        super.setVersion(version);
    }

    public String getName() {
        return getCoordinate(NAME);
    }

    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    @Override
    public ComparableVersion getNativeVersion() {
        String versionLocal = getVersion();
        if (versionLocal == null) {
            return null;
        }
        return new ComparableVersion(versionLocal);
    }

    @Override
    public String convertToPath(HelmCoordinates c) {
        return c.getId();
    }

    @Override
    public URI convertToResource(HelmCoordinates c) {
        return URI.create(convertToPath(c));
    }

}
