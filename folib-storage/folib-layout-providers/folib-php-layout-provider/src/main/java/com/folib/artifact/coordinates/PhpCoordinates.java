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
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class is an {@link ArtifactCoordinates} implementation for php
 * artifacts. <br>
 * See <a href="https://getcomposer.org/doc/04-schema.md#json-schema">Official php package
 * specification</a>.
 *
 * @author veadan
 */
@NodeEntity(Vertices.PHP_COORDINATES)
@XmlRootElement(name = "PhpCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = PhpCoordinates.LAYOUT_NAME, alias = PhpCoordinates.LAYOUT_ALIAS)
public class PhpCoordinates extends LayoutCoordinatesEntity<PhpCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "php";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;

    public static final String PHP_VERSION_REGEX = "(\\d+)\\.(\\d+)(?:\\.)?(\\d*)(\\.|-|\\+)?([0-9A-Za-z-~.]*)?";

    private static final String COMPOSER_P = "p/";

    public static final String COMPOSER_P2 = "p2/";

    public static final String COMPOSER_DISTS = "dists/";

    public static final String PHP_NAME_REGEX = "[a-z0-9]([_.-]?[a-z0-9]+)*/[a-z0-9](([_.~-]?|-{0,2})[a-z0-9]+)*";

    public static final String PHP_EXTENSION_REGEX = "(tar|tar.gz|tar.bz2|zip|json)";

    public static final String PHP_COMPOSER_VERSION_PREFIX_REGEX = "([a-z0-9]+/)?";

    public static final String PHP_COMPOSER_VERSION_REGEX = "([a-z0-9]+)?";

    public static final String PHP_PACKAGE_PATH_REGEX = PHP_COMPOSER_VERSION_PREFIX_REGEX + "(" + PHP_NAME_REGEX + ")\\." + PHP_EXTENSION_REGEX;

    public static final String PHP_PACKAGE_P_PATH_REGEX = PHP_COMPOSER_VERSION_PREFIX_REGEX + "([\\s\\S]*)\\." + PHP_EXTENSION_REGEX;

    private static final Pattern PHP_COMPOSER_VERSION_PATTERN = Pattern.compile(PHP_COMPOSER_VERSION_REGEX);

    private static final Pattern PHP_NAME_PATTERN = Pattern.compile(PHP_NAME_REGEX);

    private static final Pattern PHP_PATH_PATTERN = Pattern.compile(PHP_PACKAGE_PATH_REGEX);

    private static final Pattern PHP_P_PATH_PATTERN = Pattern.compile(PHP_PACKAGE_P_PATH_REGEX);

    private static final Pattern PHP_EXTENSION_PATTERN = Pattern.compile(PHP_EXTENSION_REGEX);

    private static final String PACKAGES = "packages";

    private static final String POINT = ".";

    public static final String JSON = "json";

    public static final String DEFAULT_PACKAGES = PACKAGES + POINT + JSON;

    private static final String COMPOSER_VERSION = "composerVersion";

    private static final String NAME = "name";

    public static final String DESCRIPTION = "description";

    private static final String EXTENSION = "extension";

    public PhpCoordinates() {
        resetCoordinates(NAME, DESCRIPTION, EXTENSION);
    }

    public PhpCoordinates(String composerVersion, String name, String description,
                          String extension) {
        setComposerVersion(composerVersion);
        setName(name);
        setDescription(description);
        setExtension(extension);
    }

    public PhpCoordinates(String composerVersion, String name,
                          String extension) {
        setComposerVersion(composerVersion);
        setName(name);
        setExtension(extension);
    }

    public static PhpCoordinates parse(String path) {
        if (DEFAULT_PACKAGES.equals(path)) {
            return new PhpCoordinates("", PACKAGES, JSON);
        }
        if (path.startsWith(COMPOSER_P)) {
            //p
            Matcher matcher = PHP_P_PATH_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(),
                    String.format("Illegal artifact path [%s], PHP artifact path should be in the form of " +
                                    "'{packageName).{extension}'.",
                            path));
            String composerVersion = matcher.group(1);
            if (StringUtils.isNotBlank(composerVersion)) {
                composerVersion = composerVersion.replace("/", "");
            }
            String name = matcher.group(2);
            String extension = matcher.group(3);
            return new PhpCoordinates(composerVersion, name, extension);
        } else if (path.startsWith(COMPOSER_P2)) {
            //p2
            Matcher matcher = PHP_PATH_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(),
                    String.format("Illegal artifact path [%s], PHP artifact path should be in the form of " +
                                    "'{packageName).{extension}'.",
                            path));
            String composerVersion = matcher.group(1);
            if (StringUtils.isNotBlank(composerVersion)) {
                composerVersion = composerVersion.replace("/", "");
            }
            String name = matcher.group(2);
            String extension = matcher.group(6);
            return new PhpCoordinates(composerVersion, name, extension);
        } else if (path.startsWith(COMPOSER_DISTS)) {
            //dists
            String composerVersion = COMPOSER_DISTS.replace("/", "");
            String name = path.substring(COMPOSER_DISTS.length(), path.lastIndexOf("."));
            String extension = path.substring(path.lastIndexOf(".") + 1);
            return new PhpCoordinates(composerVersion, name, extension);
        }
        int pointIndex = path.lastIndexOf(".");
        String name = path.substring(0, pointIndex);
        String extension = path.substring(pointIndex + 1);
        return new PhpCoordinates("", name, extension);
    }

    public static String calculatePackageId(String vendorName, String projectName) {
        Assert.isTrue(StringUtils.isNotBlank(vendorName), "vendorName is blank");
        Assert.isTrue(StringUtils.isNotBlank(projectName), "projectName is blank");
        return String.format("%s/%s", vendorName, projectName);
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        if (!PACKAGES.equals(name) && StringUtils.isNotBlank(getComposerVersion()) && getComposerVersion().startsWith(COMPOSER_P2)) {
            Matcher matcher = PHP_NAME_PATTERN.matcher(name);
            Assert.isTrue(matcher.matches(),
                    String.format("The artifact's name [%s] should follow the PHP specification " +
                                    "(https://getcomposer.org/doc/04-schema.md#name).",
                            name));
        }
        setCoordinate(NAME, name);
    }

    @ArtifactLayoutCoordinate
    public String getDescription() {
        return getCoordinate(DESCRIPTION);
    }

    public void setDescription(String description) {
        if (StringUtils.isBlank(description)) {
            return;
        }
        setCoordinate(DESCRIPTION, description);
    }

    @ArtifactLayoutCoordinate
    public String getComposerVersion() {
        return getCoordinate(COMPOSER_VERSION);
    }

    public void setComposerVersion(String composerVersion) {
        if (StringUtils.isBlank(composerVersion)) {
            return;
        }
        Matcher matcher = PHP_COMPOSER_VERSION_PATTERN.matcher(composerVersion);
        Assert.isTrue(matcher.matches(),
                String.format("The composer api version [%s] should the p or p2",
                        composerVersion));
        setCoordinate(COMPOSER_VERSION, composerVersion);
    }

    @Override
    public String getId() {
        return getName();
    }

    public void setId(String id) {
        setName(id);
    }

    @Override
    public SemanticVersion getNativeVersion() {
        return null;
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    public void setExtension(String extension) {
        Matcher matcher = PHP_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension");

        setCoordinate(EXTENSION, extension);
    }

    @Override
    public String convertToPath(PhpCoordinates c) {
        String path = String.format("%s.%s", c.getName(), c.getExtension());
        if (StringUtils.isNotBlank(c.getComposerVersion())) {
            path = String.format("%s/%s.%s", c.getComposerVersion(), c.getName(), c.getExtension());
        }
        return path;
    }

    @Override
    public URI convertToResource(PhpCoordinates c) {
        return URI.create(convertToPath(c));
    }

}
