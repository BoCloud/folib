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

import cn.hutool.core.io.FileUtil;
import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 1/3/2024 15:36
 */
@NodeEntity(Vertices.GO_COORDINATES)
@XmlRootElement(name = "GoCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = GoCoordinates.LAYOUT_NAME, alias = GoCoordinates.LAYOUT_ALIAS)
public class GoCoordinates extends LayoutCoordinatesEntity<GoCoordinates, SemanticVersion> {
    public static final String LAYOUT_NAME = "go";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final Logger logger = LoggerFactory.getLogger(GoCoordinates.class);


    public static final String NAME = "name";
    public static final String VERSION = "version";
    public static final String EXTENSION = "extension";

    public static final String GO_EXTENSION_REGEX = "(zip)";
    private static final Pattern GO_EXTENSION_PATTERN = Pattern.compile(GO_EXTENSION_REGEX);

    public GoCoordinates() {
        resetCoordinates(NAME, EXTENSION, VERSION);
    }

    public GoCoordinates(String name, String extension, String version) {
        this();
        setName(name);
        setExtension(extension);
        setVersion(version);
    }


    public static GoCoordinates parse(String path) {
        String[] parts = path.split("/@");

        Assert.isTrue(parts.length == 2, String.format("Illegal artifact path [%s]", path));

        String modulePath = parts[0];
        String after = parts[1];

        Assert.isTrue(after.startsWith("v/"), String.format("Illegal artifact path [%s]", path));
        // remove 'v/'
        after = after.substring(2);

        String extName = FileUtil.extName(after);
        String suffix = "." + extName;

        Assert.isTrue(Objects.equals(".zip", suffix), String.format("Illegal artifact path [%s]", path));

        String version = after.substring(0, after.lastIndexOf(suffix));

        return new GoCoordinates(modulePath, extName, version);
    }

    @Override
    public String getId() {
        return getName();
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        setCoordinate(NAME, name);
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    private void setExtension(String extension) {
        Matcher matcher = GO_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension:" + extension);

        setCoordinate(EXTENSION, extension);
    }

    @Override
    public void setVersion(String version) {
        super.setVersion(version);
    }

    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    @Override
    public String convertToPath(GoCoordinates artifactCoordinates) {
        return String.format("%s/@v/%s.%s", artifactCoordinates.getName(), artifactCoordinates.getVersion(), artifactCoordinates.getExtension());
    }

}
