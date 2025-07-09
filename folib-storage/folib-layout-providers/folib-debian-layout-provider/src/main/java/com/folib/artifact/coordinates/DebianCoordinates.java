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
import com.folib.constant.DebianConstant;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import lombok.extern.slf4j.Slf4j;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

import java.util.regex.Matcher;

/**
 * @author veadan
 * <p>
 * 1.下载包索引文件 /dists/{distribution}/{component}/binary-{architecture}/Packages.gz
 * 2.下载包 路径   /pool/[component]/[first-letter]/[package-name]/[package-filename]
 */

@Slf4j
@NodeEntity(Vertices.DEBIAN_COORDINATES)
@XmlRootElement(name = "DebianCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = DebianConstant.LAYOUT_NAME, alias = DebianConstant.LAYOUT_ALIAS)
public class DebianCoordinates
        extends LayoutCoordinatesEntity<DebianCoordinates, SemanticVersion> {

    public DebianCoordinates() {
        resetCoordinates(DebianConstant.ARCHITECTURE, DebianConstant.COMPONENT, DebianConstant.DISTRIBUTION);
    }

    public DebianCoordinates(String component, String name, String extension) {
        setName(name);
        if(component!=null){
            setComponent(component);
        }
        setExtension(extension);
    }

    public void setDistribution(String distribution) {
        setCoordinate(DebianConstant.DISTRIBUTION, distribution);
    }
    public String getDistribution() {
        return getCoordinate(DebianConstant.DISTRIBUTION);
    }
    public void setComponent(String component) {
        setCoordinate(DebianConstant.COMPONENT, component);
    }
    public String getComponent() {
        return getCoordinate(DebianConstant.COMPONENT);
    }
    public void setArchitecture(String architecture) {
        setCoordinate(DebianConstant.ARCHITECTURE, architecture);
    }
    public String getArchitecture() {
        return getCoordinate(DebianConstant.ARCHITECTURE);
    }


    public void setExtension(String extension) {
        setCoordinate(DebianConstant.EXTENSION, extension);
    }

    public String getExtension() {
        return getCoordinate(DebianConstant.EXTENSION);
    }

    public void setName(String name) {
        setCoordinate(DebianConstant.NAME, name);
    }

    public String getName() {
        return getCoordinate(DebianConstant.NAME);
    }

    public void setFileName(String fileName) {
        setCoordinate(DebianConstant.FILENAME, fileName);
    }

    public String getFileName() {
        return getCoordinate(DebianConstant.FILENAME);
    }

    @Override
    public String getId() {
        return getCoordinate(DebianConstant.NAME);
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

    @Override
    public String convertToPath(DebianCoordinates c) {
        String path = null;
        //没有发行版是具体的包
        if (DebianConstant.DEFAULT_EXTENSION.equals(c.getExtension())) {
            path=c.getName();
//            if(c.getComponent()==null){
//                path=c.getName();
//            }else {
//                String subName = getLibPackagePath(c.getFileName());
//                path = String.format("%s/%s/%s/%s/%s", DebianConstant.DEB_PREFIX, c.getComponent(), subName, c.getFileName(), c.getName());
//            }

        } else if (DebianConstant.PACKAGE_EXTENSION.equals(c.getExtension())) {
            path = String.format("%s/%s/%s/binary-%s/%s", DebianConstant.PACKAGE_PREFIX, getDistribution(), getComponent(), getArchitecture(), getName());
        }
        return path;
    }

    private String getLibPackagePath(String baseName) {
        if (!baseName.startsWith("lib")) {
            return baseName.charAt(0) + "";
        }
        if (baseName.length() <= 4) {
            return "lib";
        }
        return baseName.substring(0, 4);
    }

    public static DebianCoordinates parse(String path) {
        DebianCoordinates coordinates;
        if (path.startsWith("pool")) {
            Matcher matcher = DebianConstant.PATH_PATTERN.matcher(path);
            if(matcher.matches()) {
                String component = matcher.group(1);
                String fileName = matcher.group(2);
                String version = matcher.group(3);
                log.info("path is component:{},filename:{},version:{}", component, fileName, version);
                coordinates = DebianCoordinates.of(null, path, DebianConstant.DEFAULT_EXTENSION);
                coordinates.setVersion(version);
                coordinates.setFileName(fileName);
            }else {
                String[] split = path.split("/");
                String component = "main";
                if(split.length >1) {
                    component=split[1];
                }
                String name = path.substring(path.lastIndexOf('/') + 1);
                coordinates = DebianCoordinates.of(null, path, DebianConstant.DEFAULT_EXTENSION);
                coordinates.setVersion("1.0.0");
                coordinates.setFileName(name);

            }

        } else if(path.endsWith(DebianConstant.DEFAULT_EXTENSION)) {
            Matcher matcher = DebianConstant.CUSTOM_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(), "Invalid debian package path");
            String packageName = matcher.group(1) != null ? matcher.group(1) : matcher.group(5);
            String version = matcher.group(2) != null ? matcher.group(2) : matcher.group(6);
            String architecture = matcher.group(3) != null ? matcher.group(3) : matcher.group(7);
            String extension = matcher.group(4) != null ? matcher.group(4) : matcher.group(8);
            coordinates = new DebianCoordinates();
            coordinates.setFileName(packageName);
            coordinates.setVersion(version);
            coordinates.setArchitecture(architecture);
            coordinates.setExtension(extension);
            coordinates.setName(path);
        }else {
            Matcher matcher = DebianConstant.PACKAGE_PATTERN.matcher(path);
            Assert.isTrue(matcher.matches(), "Invalid debian package path"+path);
            String codename = matcher.group("codename");
            String component = matcher.group("component");
            String architecture = matcher.group("architecture");
            String name = matcher.group("filename");

            coordinates = DebianCoordinates.of(component, name, DebianConstant.PACKAGE_EXTENSION);
            coordinates.setDistribution(codename);
            coordinates.setArchitecture(architecture);
            coordinates.setVersion("1.0.0");
        }
        return coordinates;
    }

    public static DebianCoordinates of(String component, String name, String extension) {
        return new DebianCoordinates(component, name, extension);
    }
}
