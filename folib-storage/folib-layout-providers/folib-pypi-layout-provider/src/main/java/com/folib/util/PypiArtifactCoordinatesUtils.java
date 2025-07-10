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
package com.folib.util;

import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.domain.PypiPackageInfo;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to handle parsing of PyPi filename string
 *
 * @author alecg956
 */
@Slf4j
public class PypiArtifactCoordinatesUtils {

    private static Pattern PACKAGE_VERSION_PATTERN = Pattern.compile(PypiPackageInfo.VERSION_FORMAT,
            Pattern.CASE_INSENSITIVE);

    private static Pattern PACKAGE_DISTRIBUTION_NAME_PATTERN = Pattern.compile(PypiPackageInfo.DISTRIBUTION_NAME_FORMAT,
            Pattern.CASE_INSENSITIVE);

    /**
     * If optional build parameter is not found in the wheel package filename the empty string is specified for build_tag
     * in the construction of a PypiArtifactCoordinates object
     * <p>
     * Format of Wheel: {distribution}-{version}(-{build tag})?-{python tag}-{abi tag}-{platform tag}.whl.
     * Format of source: {distribution}-{version}.tar.gz
     *
     * @param path The filename of the PyPi artifact
     * @return Returns a PypiArtifactCoordinate object with all coordinates in the filename set
     */
    public static PypiCoordinates parse(String path) {
        if (path.endsWith(".html")) {
            PypiCoordinates pypiArtifactCoordinates = new PypiCoordinates();
            pypiArtifactCoordinates.setPath(path);
            pypiArtifactCoordinates.setId(path);
            return pypiArtifactCoordinates;
        }
        if (PypiCoordinates.EXTENSION_LIST.stream().noneMatch(path::endsWith)) {
            String message = String.format("The artifact packaging can be only %s path [%s]", String.join(" or ", PypiCoordinates.EXTENSION_LIST), path);
            log.info(message);
            throw new IllegalArgumentException(message);
        }

        String fileName = FilenameUtils.getName(path);
        PypiCoordinates pypiArtifactCoordinates = PypiCoordinates.WHEEL_EXTENSION_LIST.stream().noneMatch(path::endsWith) ? parseSourcePackage(fileName) :
                parseWheelPackage(fileName);
        pypiArtifactCoordinates.setPath(path);
        return pypiArtifactCoordinates;
    }


    private static PypiCoordinates parseSourcePackage(String path) {
        try {
            String extension = FilenameUtils.getExtension(path);
            if (path.endsWith(PypiCoordinates.FULL_TAR_GZ_SUFFIX)) {
                extension = PypiCoordinates.TAR_GZ_SUFFIX;
            }
            String fullExtension = GlobalConstants.POINT + extension;
            String packageNameWithoutExtension = path.substring(0, path.lastIndexOf(fullExtension));
            String distribution = packageNameWithoutExtension.substring(0,
                    packageNameWithoutExtension.lastIndexOf("-"));
            String version = packageNameWithoutExtension.substring(packageNameWithoutExtension.indexOf("-") + 1);

            Matcher matcher = PACKAGE_VERSION_PATTERN.matcher(version);
            if (!matcher.matches()) {
                log.warn(String.format("Invalid version [%s] for source package.", version));
            }

            matcher = PACKAGE_DISTRIBUTION_NAME_PATTERN.matcher(distribution);
            if (!matcher.matches()) {
                throw new IllegalArgumentException(String.format("Invalid name [%s] for source package.", distribution));
            }

            return new PypiCoordinates(distribution, version, extension);
        } catch (IllegalArgumentException iae) {
            throw iae;
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid source package name.");
        }
    }

    private static PypiCoordinates parseWheelPackage(String path) {
        String extension = FilenameUtils.getExtension(path);
        String fullExtension = GlobalConstants.POINT + extension;
        String[] splitArray = path.split("-");
        boolean isWhl = PypiCoordinates.WHL.equalsIgnoreCase(extension);
        // check for invalid file format
        if (isWhl && splitArray.length != 5 && splitArray.length != 6) {
            throw new IllegalArgumentException(String.format("Invalid wheel package name specified path [%s]", path));
        }

        String distribution = splitArray[0];
        String version = splitArray[1];
        String build = null;
        String languageImplementationVersion = null;
        String abi = null;
        String platform = null;

        // build tag not included
        if (isWhl) {
            if (splitArray.length == 5) {
                languageImplementationVersion = splitArray[2];
                abi = splitArray[3];
                platform = splitArray[4].substring(0, splitArray[4].indexOf(fullExtension));

            }
            // build tag is included
            else if (splitArray.length == 6) {
                build = splitArray[2];
                languageImplementationVersion = splitArray[3];
                abi = splitArray[4];
                platform = splitArray[5].substring(0, splitArray[5].indexOf(fullExtension));
            }
        } else {
            languageImplementationVersion = StringUtils.removeEnd(splitArray[2], PypiCoordinates.EGG);
            if (splitArray.length >= 4) {
                platform = StringUtils.removeEnd(splitArray[3], PypiCoordinates.EGG);
            }
        }

        return new PypiCoordinates(distribution,
                version,
                build,
                languageImplementationVersion,
                abi,
                platform, extension);
    }

    public static void main(String[] args) {
        String version = "2005e";
        Matcher matcher = PACKAGE_VERSION_PATTERN.matcher(version);
        if (!matcher.matches()) {
            throw new IllegalArgumentException(String.format("Invalid version [%s] for source package.", version));
        }
    }
}
