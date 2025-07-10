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
import com.folib.util.PypiArtifactCoordinatesUtils;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;

import java.net.URI;
import java.util.List;

/**
 * This class is an {@link ArtifactCoordinates} implementation for pypi artifacts
 * <p>
 * Proper path for this coordinates is in the format of:
 * {distribution}-{version}(-{build tag})?-{python tag}-{abi tag}-{platform tag}.whl.
 * for wheel packages and {distribution}-{version}.tar.gz for source packages
 * Examples: distribution-1.0.1-1-py27-none-any.whl, distribution-1.0.1.tar.gz
 *
 * @author alecg956
 */
@NodeEntity(Vertices.PYPI_COORDINATES)
@XmlRootElement(name = "PypiCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = PypiCoordinates.LAYOUT_NAME, alias = PypiCoordinates.LAYOUT_ALIAS)
public class PypiCoordinates
        extends LayoutCoordinatesEntity<PypiCoordinates, SemanticVersion> {

    public static final String LAYOUT_NAME = "PyPi";

    public static final String LAYOUT_ALIAS = "pypi";

    public static final String DISTRIBUTION = "distribution";

    public static final String VERSION = "version";

    public static final String BUILD = "build";

    public static final String LANGUAGE_IMPLEMENTATION_VERSION = "languageImplementationVersion";

    public static final String ABI = "abi";

    public static final String PLATFORM = "platform";

    public static final String PACKAGING = "packaging";

    public static final List<String> SOURCE_EXTENSION_LIST = Lists.newArrayList("exe", "tar.gz", "bz2", "rpm", "deb", "zip", "tgz", "dmg", "msi");

    public static final List<String> WHEEL_EXTENSION_LIST = Lists.newArrayList("whl", "egg");

    public static final List<String> EXTENSION_LIST = Lists.newArrayList("exe", "tar.gz", "bz2", "rpm", "deb", "zip", "tgz", "egg", "dmg", "msi",
            "whl");

    public static final String FULL_TAR_GZ_SUFFIX = ".tar.gz";

    public static final String TAR_GZ_SUFFIX = "tar.gz";

    public static final String PATH = "path";

    public static final String WHL = "whl";

    public static final String EGG = ".egg";

    public PypiCoordinates() {
        resetCoordinates(DISTRIBUTION,
                VERSION,
                BUILD,
                LANGUAGE_IMPLEMENTATION_VERSION,
                ABI,
                PLATFORM,
                PACKAGING);
    }

    /**
     * This method takes in all artifact coordinates of a PyPi package filename, with build being
     * the empty string if it is not included in the filename
     *
     * @param distribution                  Uniquely identifying artifact coordinate (required)
     * @param version                       Packages current version (required)
     * @param build                         Build_tag parameter (optional)
     * @param languageImplementationVersion Language and Implementation version argument (optional)
     * @param abi                           ABI tag parameter (optional)
     * @param platform                      Platform tag parameter (optional)
     * @param packaging                     Packaging of artifact (required)
     */
    public PypiCoordinates(String distribution,
                           String version,
                           String build,
                           String languageImplementationVersion,
                           String abi,
                           String platform,
                           String packaging) {
        this();

        if (StringUtils.isBlank(packaging)) {
            throw new IllegalArgumentException("The packaging field is mandatory.");
        }

        if (!SOURCE_EXTENSION_LIST.contains(packaging) && !WHEEL_EXTENSION_LIST.contains(packaging)) {
            throw new IllegalArgumentException("The artifact has incorrect packaging");
        }

        if (SOURCE_EXTENSION_LIST.contains(packaging)) {
            if (StringUtils.isBlank(distribution) || StringUtils.isBlank(version)) {
                throw new IllegalArgumentException(
                        "The distribution and version fields are mandatory for source package.");
            }
        }

        if (WHL.equalsIgnoreCase(packaging)) {
            if (StringUtils.isBlank(distribution) || StringUtils.isBlank(version) || StringUtils.isBlank(platform)
                    || StringUtils.isBlank(languageImplementationVersion) || StringUtils.isBlank(abi)) {
                throw new IllegalArgumentException("The distribution, version, languageImplementationVersion, abi, and " +
                        "platform fields are mandatory for wheel package.");
            }

            if (!StringUtils.isBlank(build) && !Character.isDigit(build.charAt(0))) {
                throw new IllegalArgumentException("Illegal build tag!");
            }
        }

        setId(distribution);
        setVersion(version);
        setBuild(build);
        setLanguageImplementationVersion(languageImplementationVersion);
        setAbi(abi);
        setPlatform(platform);
        setPackaging(packaging);
    }

    /**
     * This method takes in all artifact coordinates of a PyPi Wheel filename, with build being
     * the empty string if it is not included in the filename
     *
     * @param distribution Uniquely identifying artifact coordinate (required)
     * @param version      Packages current version (required)
     * @param packaging    Packaging of artifact (required)
     */
    public PypiCoordinates(String distribution, String version, String packaging) {
        this(distribution, version, null, null, null, null, packaging);
    }

    /**
     * @param path The filename of the PyPi package
     * @return Returns a PyPiArtifactCoordinates object with all included coordinates set
     */
    public static PypiCoordinates parse(String path) {
        return PypiArtifactCoordinatesUtils.parse(path);
    }

    /**
     * @return Returns distribution coordinate value (serves as the unique ID)
     */
    @Override
    public String getId() {
        return getCoordinate(DISTRIBUTION);
    }

    /**
     * @param id DISTRIBUTION coordinate will take this value
     */
    public void setId(String id) {
        setCoordinate(DISTRIBUTION, id);
    }


    /**
     * @return Returns the BUILD coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getBuild() {
        return getCoordinate(BUILD);
    }

    /**
     * @param build BUILD coordinate will take this value
     */
    public void setBuild(String build) {
        setCoordinate(BUILD, build);
    }

    /**
     * @param packaging PACKAGING of artifact
     */
    public void setPackaging(String packaging) {
        setCoordinate(PACKAGING, packaging);
    }

    /**
     * @return Returns PACKAGING of artifact
     */
    public String getPackaging() {
        return getCoordinate(PACKAGING);
    }

    /**
     * @return Returns the LANGUAGE_IMPLEMENTATION_VERSION coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getLanguageImplementationVersion() {
        return getCoordinate(LANGUAGE_IMPLEMENTATION_VERSION);
    }

    /**
     * @param lang LANGUAGE_IMPLEMENTATION_VERSION takes this value
     */
    public void setLanguageImplementationVersion(String lang) {
        setCoordinate(LANGUAGE_IMPLEMENTATION_VERSION, lang);
    }

    /**
     * @return Returns the ABI coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getAbi() {
        return getCoordinate(ABI);
    }

    /**
     * @param abi ABI coordinate takes this value
     */
    public void setAbi(String abi) {
        setCoordinate(ABI, abi);
    }

    /**
     * @return Returns the PLATFORM coordinate value
     */
    @ArtifactLayoutCoordinate
    public String getPlatform() {
        return getCoordinate(PLATFORM);
    }

    /**
     * @param platform PLATFORM coordinate takes this value
     */
    public void setPlatform(String platform) {
        setCoordinate(PLATFORM, platform);
    }

    @Override
    @ArtifactLayoutCoordinate
    public String getPath() {
        return getCoordinate(PATH);
    }

    /**
     * @param path PLATFORM coordinate takes this value
     */
    public void setPath(String path) {
        setCoordinate(PATH, path);
    }

    /**
     * @return Returns the reconstructed path from the stored coordinate values
     */
    @Override
    public String convertToPath(PypiCoordinates c)
    {
        String fileName = SOURCE_EXTENSION_LIST.contains(c.getPackaging()) ? c.buildSourcePackageFileName()
                : c.buildWheelPackageFileName();
        if (StringUtils.isBlank(c.getPath()) || fileName.equals(c.getPath())) {
            return String.format("%s/%s/%s",
                    c.getId(),
                    c.getVersion(),
                    fileName);
        }
        return c.getPath();
    }

    @Override
    public URI convertToResource(PypiCoordinates artifactCoordinates) {
        String path = convertToPath(artifactCoordinates);
        return URI.create(String.format("packages/%s", path));
    }

    private String buildSourcePackageFileName() {
        return String.format("%s-%s.%s",
                getId(),
                getVersion(),
                getPackaging());
    }

    public String buildWheelPackageFileName() {
        String path;
        boolean isWhl = WHL.equalsIgnoreCase(getPackaging());
        if (isWhl) {
            if (StringUtils.isBlank(getBuild())) {
                path = String.format("%s-%s-%s-%s-%s.%s",
                        getId(),
                        getVersion(),
                        getLanguageImplementationVersion(),
                        getAbi(),
                        getPlatform(), getPackaging());
            } else {
                path = String.format("%s-%s-%s-%s-%s-%s.%s",
                        getId(),
                        getVersion(),
                        getBuild(),
                        getLanguageImplementationVersion(),
                        getAbi(),
                        getPlatform(),
                        getPackaging());
            }
        } else {
            if (StringUtils.isNotBlank(getPlatform())) {
                path = String.format("%s-%s-%s-%s.%s",
                        getId(),
                        getVersion(),
                        getLanguageImplementationVersion(),
                        getPlatform(), getPackaging());
            } else {
                path = String.format("%s-%s-%s.%s",
                        getId(),
                        getVersion(),
                        getLanguageImplementationVersion(),
                        getPackaging());
            }
        }
        return path;
    }

    public boolean isSourcePackage() {
        return SOURCE_EXTENSION_LIST.contains(getPackaging());
    }

    public String getFileName() {
        return isSourcePackage() ? buildSourcePackageFileName()
                : buildWheelPackageFileName();
    }

    /**
     * @return Returns the native version of the package
     */
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

    public static PypiCoordinates resolveName(String artifactPath)
    {
        PypiCoordinates pypiArtifactCoordinates = new PypiCoordinates();
        String[] arr = artifactPath.split(GlobalConstants.SEPARATOR);
        pypiArtifactCoordinates.setId(arr[0]);
        return pypiArtifactCoordinates;
    }

}
