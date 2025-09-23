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

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;


import static com.folib.db.schema.Properties.NAME;
import static com.folib.db.schema.Properties.PLATFORM;


@Slf4j
@NodeEntity(Vertices.CONDA_COORDINATES)
@XmlRootElement(name = "CondaArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = CondaCoordinates.LAYOUT_NAME, alias = CondaCoordinates.LAYOUT_ALIAS)
public class CondaCoordinates
        extends LayoutCoordinatesEntity<CondaCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "conda";
    public static final String LAYOUT_ALIAS = "conda";


    public CondaCoordinates() {
        resetCoordinates(PLATFORM, NAME);
    }

    public CondaCoordinates(String platform,
                            String name) {
        this();
        setPlatform(platform);
        setName(name);
    }

    @Override
    public String getId() {
        return getName();
    }

    // getters
    @ArtifactLayoutCoordinate
    public String getPlatform() {
        return getCoordinate(PLATFORM);
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }



    // setters
    private void setPlatform(String platform) {
        setCoordinate(PLATFORM, platform);
    }
    private void setName(String extension) {
        setCoordinate(NAME, extension);
    }

    public static CondaCoordinates of(String platform, String name) {
        return new CondaCoordinates(platform, name);
    }


    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    // 路径解析:
    // 文件路径: {platform}/{name}
    public static CondaCoordinates parse(String path) {
        if (path.startsWith(".trash/")) {
            path = path.substring(".trash/".length());
        }
        Assert.notNull(path, "path cannot be null");
        String[] parts = path.split("/");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid path format: " + path);
        }
        String platform = parts[0];
        String name = parts[1];
        return new CondaCoordinates(platform, name);
    }

    @Override
    public String convertToPath(CondaCoordinates artifactCoordinates) {
        String platform = artifactCoordinates.getPlatform();
        String name = artifactCoordinates.getName();
        return String.format("%s/%s", platform, name);
    }
}