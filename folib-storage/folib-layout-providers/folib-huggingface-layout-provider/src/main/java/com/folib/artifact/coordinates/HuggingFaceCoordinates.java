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
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;


import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


@Slf4j
@NodeEntity(Vertices.HUGGINGFACE_COORDINATES)
@XmlRootElement(name = "HuggingFaceCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = HuggingFaceCoordinates.LAYOUT_NAME, alias = HuggingFaceCoordinates.LAYOUT_ALIAS)
public class HuggingFaceCoordinates extends LayoutCoordinatesEntity<HuggingFaceCoordinates, HuggingFaceCoordinates> {

    public static final String LAYOUT_NAME = "HuggingFace";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;
    private static final String PATH = "path";

    private static final String SCOPE = "scope";
    private static final String NAME = "name";

    private static final String VERSION = "version";
    private static final String TIMESTAMP = "timestamp";
    private static final String FILENAME = "filename";
    private static final String EXTENSION = "extension";
    public static final String PATH_ORG_REGEX = "models/([^/]+)/([^/]+)/([^/]+)/(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z)/([^/]+(?:/[^/]+)*)";
    public static final String PATH_REGEX = "models/([^/]+)/([^/]+)/(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z)/([^/]+(?:/[^/]+)*)";
    public final String suffix = "\\.(\\w+)$";

    private static final Pattern HFML_ORG_PATH_PATTERN = Pattern.compile(PATH_ORG_REGEX);

    private static final Pattern HFML_PATH_PATTERN = Pattern.compile(PATH_REGEX);


    public HuggingFaceCoordinates() {
        resetCoordinates(PATH, SCOPE, NAME, VERSION, TIMESTAMP, FILENAME, EXTENSION);
    }

    public HuggingFaceCoordinates(String path) {
        if (HFML_ORG_PATH_PATTERN.matcher(path).matches()) {
            isOrg(path);
        } else if (HFML_PATH_PATTERN.matcher(path).matches()) {
            isModel(path);
        }else {
            setCoordinate(PATH, path);
        }
    }

    public void isOrg(String path) {

        Matcher matcher = HFML_ORG_PATH_PATTERN.matcher(path);
        String msg = String.format("Illegal artifact path [%s], HuggingFaceML artifact path should be in the form of " +
                "'models/{organization}/{modelName}/{revision}/{timestamp}/{filename:.+}'.", path);
        Assert.isTrue(matcher.matches(), msg);
        String spope = matcher.group(1);
        String name = matcher.group(2);
        String version = matcher.group(3);
        String timestamp = matcher.group(4);

        String filename = Paths.get(path).getFileName().toString();

        Matcher matcher1 = Pattern.compile(suffix).matcher(filename);
        String extension = null;
        if (matcher1.find()) {
            extension = matcher1.group(1);
        }

        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) ) {
            log.warn("Path [{}] name [{}] version [{}] extension [{}] pattern [{}] parse error", path, name, version, extension, HFML_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }

        setCoordinate(PATH, path);
        setCoordinate(SCOPE, spope);
        super.setVersion(version);
        setCoordinate(FILENAME, filename);
        setCoordinate(EXTENSION, extension);
        setCoordinate(TIMESTAMP, timestamp);
    }

    public void isModel(String path) {

        Matcher matcher = HFML_PATH_PATTERN.matcher(path);
        String msg = String.format("Illegal artifact path [%s], HuggingFaceML artifact path should be in the form of " +
                "'models/{modelName}/{revision}/{timestamp}/{filename:.+}'.", path);
        Assert.isTrue(matcher.matches(), msg);

        String name = matcher.group(1);
        String version = matcher.group(2);
        String timestamp = matcher.group(3);
        String filename = Paths.get(path).getFileName().toString();
        Matcher matcher1 = Pattern.compile(suffix).matcher(filename);
        String extension = null;
        if (matcher1.find()) {
            extension = matcher1.group(1);
        }

        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) || StringUtils.isBlank(filename)) {
            log.warn("Path [{}] name [{}] version [{}] filename [{}] pattern [{}] parse error", path, name, version, extension, HFML_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }
        setCoordinate(PATH, path);
        super.setVersion(version);
        setCoordinate(FILENAME, filename);
        setCoordinate(EXTENSION, extension);
        setCoordinate(TIMESTAMP, timestamp);
    }

    public String getId() {
        return getCoordinate(PATH);
    }

    public void setId(String id) {
        setCoordinate(PATH, id);
    }


    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "path")
    public String getPath() {
        return getCoordinate(PATH);
    }

    @Override
    public HuggingFaceCoordinates getNativeVersion() {
        return null;
    }

    @Override
    public String convertToPath(HuggingFaceCoordinates artifactCoordinates) {
        return artifactCoordinates.getId();
    }
}
