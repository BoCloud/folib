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

import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.constant.GlobalConstants;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.NpmLayoutProvider;
import com.folib.util.UriUtils;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import java.net.URI;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class is an {@link ArtifactCoordinates} implementation for npm
 * artifacts. <br>
 * See <a href="https://docs.npmjs.com/files/package.json">Official npm package
 * specification</a>.
 *
 * @author veadan
 */
@Slf4j
@NodeEntity(Vertices.NPM_COORDINATES)
@XmlRootElement(name = "NpmCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = NpmCoordinates.LAYOUT_NAME, alias = NpmCoordinates.LAYOUT_ALIAS)
public class NpmCoordinates extends LayoutCoordinatesEntity<NpmCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "npm";

    public static final String LAYOUT_ALIAS = LAYOUT_NAME;

    public static final String NPM_BINARY_PATH_PREFIX = "-/binary/";

    public static final String NPM_VERSION_REGEX = "(\\d+)\\.(\\d+)(?:\\.)?(\\d*)(\\.|-|\\+)?([0-9A-Za-z-.]*)?";

    public static final String NPM_NAME_REGEX = "[a-zA-Z0-9][\\w-.]*";

    public static final String NPM_EXTENSION_REGEX = "(tgz|json|har|json5)";

    public static final String NPM_PACKAGE_PATH_REGEX = "(@?" + NPM_NAME_REGEX + ")/(" + NPM_NAME_REGEX + ")/(" +
            NPM_VERSION_REGEX + ")/" + NPM_NAME_REGEX + "(-(" +
            NPM_VERSION_REGEX + "))?\\." + NPM_EXTENSION_REGEX;

    public static final String NPM_PACKAGE_NAME_REGEX = NPM_NAME_REGEX + "-(" +
            NPM_VERSION_REGEX + ")\\." + NPM_EXTENSION_REGEX;

    private static final Pattern NPM_PACKAGE_NAME_PATTERN = Pattern.compile(NPM_PACKAGE_NAME_REGEX);

    public static final Pattern NPM_NAME_PATTERN = Pattern.compile(NPM_NAME_REGEX);

    private static final Pattern NPM_PATH_PATTERN = Pattern.compile(NPM_PACKAGE_PATH_REGEX);

    public static final Pattern NPM_EXTENSION_PATTERN = Pattern.compile(NPM_EXTENSION_REGEX);

    public static final String SCOPE = "scope";

    public static final String NAME = "name";

    public static final String VERSION = "version";

    public static final String EXTENSION = "extension";

    public static final String DISTRIBUTION = "distribution";

    public static final List<String> NPM_EXTENSION_LIST = Lists.newArrayList(".tgz", ".har");

    public NpmCoordinates() {
        resetCoordinates(SCOPE, NAME, VERSION, EXTENSION);
    }

    public NpmCoordinates(String scope,
                          String name,
                          String version,
                          String extension) {
        setScope(scope);
        setName(name);
        setVersion(version);
        setExtension(extension);
    }

    public NpmCoordinates(String scope,
                          String name,
                          String version,
                          String extension,
                          String distribution) {
        if (StringUtils.isNotBlank(name)) {
            setCoordinate(NAME, name);
        }
        if (StringUtils.isNotBlank(version)) {
            super.setVersion(version);
        }
        if (StringUtils.isNotBlank(extension)) {
            setCoordinate(EXTENSION, extension);
        }
        setDistribution(distribution);
    }

    @ArtifactLayoutCoordinate
    public String getScope() {
        return getCoordinate(SCOPE);
    }

    public void setScope(String scope) {
        if (StringUtils.isBlank(scope)) {
            return;
        }
        Assert.isTrue(scope.startsWith("@"), "Scope should starts with '@'.");
        setCoordinate(SCOPE, scope);
    }

    @ArtifactLayoutCoordinate
    public String getName() {
        return getCoordinate(NAME);
    }

    public void setName(String name) {
        Matcher matcher = NPM_NAME_PATTERN.matcher(name);
        Assert.isTrue(matcher.matches(),
                String.format("The artifact's name [%s] should follow the NPM specification " +
                                "(https://docs.npmjs.com/files/package.json#name).",
                        name));

        setCoordinate(NAME, name);
    }

    @Override
    public String getId() {
        if (getScope() == null) {
            return getName();
        }

        return String.format("%s/%s", getScope(), getName());
    }

    public void setId(String id) {
        setName(id);
    }

    @Override
    public void setVersion(String version) {
        SemanticVersion.parse(version);
        super.setVersion(version);
    }

    public void setExtension(String extension) {
        Matcher matcher = NPM_EXTENSION_PATTERN.matcher(extension);
        Assert.isTrue(matcher.matches(), "Invalid artifact extension");

        setCoordinate(EXTENSION, extension);
    }

    @ArtifactLayoutCoordinate
    public String getExtension() {
        return getCoordinate(EXTENSION);
    }

    @ArtifactLayoutCoordinate
    public String getDistribution() {
        return getCoordinate(DISTRIBUTION);
    }

    public void setDistribution(String distribution) {
        setCoordinate(DISTRIBUTION, distribution);
    }

    @Override
    public String convertToPath(NpmCoordinates c) {
        if (StringUtils.isNotBlank(c.getDistribution())) {
            return c.getDistribution();
        }
        return String.format("%s/%s/%s/%s", c.getGroup(), c.getName(), c.getVersion(), c.getArtifactFileName());
    }

    @Override
    public URI convertToResource(NpmCoordinates c) {
        if (StringUtils.isNotBlank(c.getDistribution())) {
            return URI.create(c.getDistribution());
        }
        String path = convertToPath(c);
        if (path.endsWith(NpmLayoutProvider.PACKAGE_JSON)) {
            return URI.create(String.format("%s/-/%s-%s.%s", c.getId(), "package", c.getVersion(), "json"));
        } else if (path.endsWith(NpmLayoutProvider.OH_PACKAGE_JSON)) {
            return URI.create(String.format("%s/-/%s-%s.%s", c.getId(), "oh-package", c.getVersion(), "json5"));
        }
        return URI.create(String.format("%s/-/%s", c.getId(), c.getArtifactFileName()));
    }

    public String getGroup() {
        String scopeLocal = getScope();
        String nameLocal = getName();

        return scopeLocal == null ? nameLocal : scopeLocal;
    }

    public String getArtifactFileName() {
        if ("json".equals(getExtension())) {
            return "package.json";
        } else if ("json5".equals(getExtension())) {
            return "oh-package.json5";
        }
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

    public static NpmCoordinates parse(String path) {
        if (path.startsWith(NPM_BINARY_PATH_PREFIX)) {
            //binary
            String artifactPath = path.substring(NPM_BINARY_PATH_PREFIX.length());
            String[] arr = artifactPath.split(GlobalConstants.SEPARATOR);
            String name = arr[0];
            String version = arr[1];
            String extension = FilenameUtils.getExtension(path);
            return new NpmCoordinates(null, name, version, extension, path);
        }
        Matcher matcher = NPM_PATH_PATTERN.matcher(path);

        Assert.isTrue(matcher.matches(),
                String.format("Illegal artifact path [%s], NPM artifact path should be in the form of " +
                                "'{artifactGroup}/{artifactName}/{artifactVersion}/{artifactFile}'.",
                        path));

        String group = matcher.group(1);
        String name = matcher.group(2);
        String version = matcher.group(3);
        String extension = matcher.group(16);

        if (group.startsWith("@")) {
            return new NpmCoordinates(group, name, version, extension);
        }

        return new NpmCoordinates(null, name, version, extension);
    }

    public static NpmCoordinates of(String packageId,
                                    String version, String packagingSuffixes) {
        if (packageId.contains("/")) {
            String[] nameSplit = packageId.split("/");

            return new NpmCoordinates(nameSplit[0], nameSplit[1], version, packagingSuffixes);
        }

        return new NpmCoordinates(null, packageId, version, packagingSuffixes);
    }

    public static String calculatePackageId(String packageScope, String packageName) {
        return packageScope == null ? packageName : String.format("%s/%s", packageScope, packageName);
    }

    public static NpmCoordinates resolveName(RepositoryPath repositoryPath, String artifactPath) {
        NpmCoordinates npmArtifactCoordinates = new NpmCoordinates();
        String[] arr = artifactPath.split(GlobalConstants.SEPARATOR);
        if (!arr[0].startsWith(GlobalConstants.AT)) {
            npmArtifactCoordinates.setName(arr[0]);
        } else {
            npmArtifactCoordinates.setScope(arr[0]);
            if (arr.length >= 2) {
                npmArtifactCoordinates.setName(arr[1]);
            }
        }
        return npmArtifactCoordinates;
    }

    public static NpmCoordinates parseByResolvePath(String path) {
        try {
            path = UriUtils.decode(path);
            path = path.replace("/-/", "/");
            String[] arr = path.split("/");
            String packageScope = "", packageName = "", packageNameWithVersion = "", version = "";
            if (path.startsWith("@")) {
                packageScope = arr[0];
                packageName = arr[1];
                packageNameWithVersion = arr[2];
            } else {
                packageName = arr[0];
                packageNameWithVersion = arr[1];
            }
            Matcher matcher = NPM_PACKAGE_NAME_PATTERN.matcher(packageNameWithVersion);
            if (matcher.matches()) {
                version = matcher.group(1);
            }
            return of(calculatePackageId(packageScope, packageName), version, FilenameUtils.getExtension(packageNameWithVersion));
        } catch (Exception ex) {
            log.error("Parse path [{}] error [{}]", path, ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

}
