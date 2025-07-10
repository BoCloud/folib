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
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class is an {@link ArtifactCoordinates} implementation for pub
 * artifacts. <br>
 * See <a href="https://dart.cn/tools/pub/pubspec">Official pub pubspec
 * specification</a>.
 *
 * @author veadan
 */
@Slf4j
@NodeEntity(Vertices.PUB_COORDINATES)
@XmlRootElement(name = "pubArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = PubCoordinates.LAYOUT_NAME, alias = PubCoordinates.LAYOUT_ALIAS)
public class PubCoordinates extends LayoutCoordinatesEntity<PubCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "pub";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;

    public static final String PUB_VERSION_REGEX = "(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?";

    public static final String PUB_NAME_REGEX = "[a-z_][a-z_\\d-]*";

    public static final String PUB_EXTENSION_REGEX = "(tar.gz)";

    public static final String PUB_EXTENSION = "tar.gz";

    public static final String PUB_PACKAGE_NAME_REGEX = "(" + PUB_NAME_REGEX + ")" + "-(" +
            PUB_VERSION_REGEX + ")\\." + PUB_EXTENSION_REGEX;

    public static final String PUB_PACKAGE_PATH_REGEX = "(" + PUB_NAME_REGEX + ")/" + PUB_PACKAGE_NAME_REGEX;

    public static final Pattern PUB_NAME_PATTERN = Pattern.compile(PUB_NAME_REGEX);

    private static final Pattern PUB_PACKAGE_NAME_PATTERN = Pattern.compile(PUB_PACKAGE_NAME_REGEX);

    private static final Pattern PUB_PATH_PATTERN = Pattern.compile(PUB_PACKAGE_PATH_REGEX);

    private static final Pattern PUB_VERSION_PATTERN = Pattern.compile(PUB_VERSION_REGEX);

    private static final Pattern PUB_EXTENSION_PATTERN = Pattern.compile(PUB_EXTENSION_REGEX);

    private static final String NAME = "name";

    private static final String VERSION = "version";

    private static final String EXTENSION = "extension";

    public PubCoordinates() {
        resetCoordinates(NAME, VERSION, EXTENSION);
    }

    public PubCoordinates(
            String name,
            String version,
            String extension) {
        setName(name);
        setVersion(version);
        setExtension(extension);
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        Matcher matcher = PUB_NAME_PATTERN.matcher(name);
        Assert.isTrue(matcher.matches(),
                String.format("The artifact's name [%s] should follow the PUB specification " +
                                "(https://dart.cn/tools/pub/pubspec#name).",
                        name));

        setCoordinate(NAME, name);
    }

    @Override
    public String getId() {
        return getName();
    }

    public void setId(String id) {
        setName(id);
    }

    @Override
    public void setVersion(String version) {
        Matcher matcher = PUB_VERSION_PATTERN.matcher(version);
        Assert.isTrue(matcher.matches(), "Invalid artifact version");
        super.setVersion(version);
    }

    public void setExtension(String extension) {
        Matcher matcher = PUB_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension");

        setCoordinate(EXTENSION, extension);
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    @Override
    public String convertToPath(PubCoordinates c) {
        return String.format("%s/%s", c.getName(), getArtifactFileName());
    }

    @Override
    public URI convertToResource(PubCoordinates c) {
        return URI.create(String.format("packages/%s/versions/%s", c.getName(), c.getArtifactFileName()));
    }

    public String getArtifactFileName() {
        return String.format("%s-%s.%s", getName(), getVersion(), getExtension());
    }

    @Override
    public SemanticVersion getNativeVersion() {
        String versionLocal = getVersion();
        if (versionLocal == null) {
            return null;
        }

        try {
            return SemanticVersion.parse(versionLocal);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    public static PubCoordinates parse(String path) {
        Matcher matcher = PUB_PATH_PATTERN.matcher(path);
        String msg = String.format("Illegal artifact path [%s], PUB artifact path should be in the form of " +
                        "'/{artifactName}/{artifactFile}'.",
                path);
        Assert.isTrue(matcher.matches(), msg);
        String name = matcher.group(1);
        String version = matcher.group(3);
        String extension = matcher.group(9);
        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) || StringUtils.isBlank(extension)) {
            log.warn("Path [{}] name [{}] version [{}] extension [{}] pattern [{}] parse error", path, name, version, extension, PUB_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }
        return new PubCoordinates(name, version, extension);
    }

    public static PubCoordinates packageNameParse(String packageName) {
        Matcher matcher = PUB_PACKAGE_NAME_PATTERN.matcher(packageName);
        String msg = String.format("Illegal artifact path [%s], PUB artifact name should be in the form of " +
                        "'{artifactFile}'.",
                packageName);
        Assert.isTrue(matcher.matches(), msg);
        String name = matcher.group(1);
        String version = matcher.group(2);
        String extension = matcher.group(8);
        if (StringUtils.isBlank(name) || StringUtils.isBlank(version) || StringUtils.isBlank(extension)) {
            log.warn("Path [{}] packageName [{}] version [{}] extension [{}] pattern [{}] parse error", packageName, name, version, extension, PUB_PATH_PATTERN.toString());
            throw new RuntimeException(msg);
        }
        return new PubCoordinates(name, version, extension);
    }

    public static PubCoordinates of(String name, String version, String packagingSuffixes) {
        return new PubCoordinates(name, version, packagingSuffixes);
    }

}
