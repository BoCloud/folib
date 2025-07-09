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

import javax.validation.constraints.NotBlank;



import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import com.folib.domain.RpmPackageArch;
import com.folib.domain.RpmPackageType;

import com.folib.util.RpmArtifactCoordinatesUtils;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.codehaus.commons.nullanalysis.NotNull;
import org.neo4j.ogm.annotation.NodeEntity;

import java.util.regex.Pattern;

/**
 * This class is an {@link ArtifactCoordinates} implementation for RPM-packages.
 *
 * There are two types of RPM packages. One of them is default binary RPM,
 * that contain prebuild binaries for your platform. Other way is source RPM (SRPM)
 * SRPM package contain source code, patches to it, and SPEC file, which describes
 * how to build the source code into a binary RPM.
 * Be attention - SRPM packages have SRC suffix instead architecture describing.
 *
 *  The canonical named package is represented to below structure:
 * {name}-{version}-{release}.{architecture}.rpm
 *
 * Examples:
 * somepackage-1.0-1.x86_64.rpm - binary distribution with Arch suffix;
 * somepackage-1.0-1.src.rpm    - SRPM package with SRC suffix;
 *
 * @author Ilya Shatalov <ilya@alov.me>
 */
@NodeEntity(Vertices.RPM_COORDINATES)
@XmlRootElement(name = "RpmArtifactCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = RpmCoordinates.LAYOUT_NAME, alias = RpmCoordinates.LAYOUT_ALIAS)
public class RpmCoordinates extends LayoutCoordinatesEntity<RpmCoordinates, RpmCoordinates>
{
    public static final String LAYOUT_NAME = "rpm";

    public static final String LAYOUT_ALIAS = "rpm";

    public static final String BASE_NAME = "base_name";

    public static final String VERSION = "version";

    public static final String RELEASE = "release";

    public static final String ARCHITECTURE = "architecture";

    public static final String PACKAGE_TYPE = "package_type";

    public static final String EXTENSION = "extension";

    public static final String DEFAULT_EXTENSION = "rpm";

    public static final String NAME = "name";

    public String path;


    public RpmCoordinates(@NotBlank String baseName,
                          @NotBlank String version,
                          @NotBlank String release,
                          @NotNull RpmPackageType packageType,
                          @NotNull RpmPackageArch arch,
                          @NotBlank String path)
    {
        this();
        setId(path);
        setBaseName(baseName);
        setVersion(version);
        setRelease(release);
        setPackageType(packageType);
        setArchitecture(arch);
        setExtension();
    }

    public RpmCoordinates(@NotBlank String baseName,
                          @NotBlank String version,
                          @NotBlank String release,
                          @NotBlank String path,
                          @NotNull RpmPackageType packageType)
    {
        this();
        setId(path);
        setBaseName(baseName);
        setVersion(version);
        setRelease(release);
        setPackageType(packageType);
        setExtension();
    }

    public RpmCoordinates()
    {
        resetCoordinates(BASE_NAME, VERSION, RELEASE, ARCHITECTURE, EXTENSION);
    }

    @Override
    public String getId()
    {
        return getCoordinate(NAME);
    }

    @Override
    public RpmCoordinates getNativeVersion() {
        return null;
    }

    public String getPath(@NotBlank String baseName,
                          @NotBlank String version,
                          @NotBlank String release,
                          @NotNull RpmPackageType packageType,
                          RpmPackageArch arch) {
        String path;
        if (RpmPackageType.SOURCE.getPostfix().equals(packageType.getPostfix())) {
            path = String.format("Packages/%s-%s-%s.%s.%s",
                    baseName,
                    version,
                    release,
                    packageType.getPostfix(),
                    DEFAULT_EXTENSION);
        } else {
            path = String.format("Packages/%s-%s-%s.%s.%s",
                    baseName,
                    version,
                    release,
                    arch.getName(),
                    DEFAULT_EXTENSION);
        }
        return path;
    }

    public void setId(String id)
    {

        setCoordinate(NAME, id);
    }
    public void setBaseName(String baseName) {
        setCoordinate(BASE_NAME, baseName);
    }
    public String getBaseName() {
        return getCoordinate(BASE_NAME);
    }

    public String getRelease()
    {
        return getCoordinate(RELEASE);
    }

    public void setRelease(String release)
    {
        setCoordinate(RELEASE, release);
    }

    public String getArchitecture()
    {
        return getCoordinate(ARCHITECTURE);
    }

    public void setArchitecture(RpmPackageArch arch)
    {
        setCoordinate(ARCHITECTURE, arch.getName());
    }

    public void setPackageType(RpmPackageType packageType)
    {
        setCoordinate(PACKAGE_TYPE, packageType.getPostfix());
    }

    public void setExtension()
    {
        setCoordinate(EXTENSION, DEFAULT_EXTENSION);
    }


    public String getPackageType()
    {
        return getCoordinate(PACKAGE_TYPE);
    }

    public String getExtension()
    {
        return getCoordinate(EXTENSION);
    }

    public void setVersion(String version){
        super.setVersion(version);
    }

    //@Override
    //public SemanticVersion getNativeVersion()
    //{
    //    String version = getVersion();
    //
    //    return version == null || version.isEmpty()
    //            ? null
    //            : SemanticVersion.parse(version);
    //}

    @Override
    public String convertToPath(RpmCoordinates c) {
        String path;
        //前缀
        String prefix = "";

        String regex = "^(.*?)-([^-]+)-([^-]+)\\.([^.]+)\\.rpm$";

        Pattern pattern = Pattern.compile(regex);
        if (c.getId().contains("Packages/")) {
            if (pattern.matcher(c.getId().replace("Packages/", "")).matches()) {
                return c.getId();
            }
            prefix = "";
        }
        if (RpmPackageType.SOURCE.getPostfix().equals(c.getPackageType())) {
            path = String.format("%s%s-%s-%s.%s.%s",
                    prefix,
                    c.getId(),
                    c.getVersion(),
                    c.getRelease(),
                    c.getPackageType(),
                    c.getExtension());
        } else {
            path=c.getId();
//            path = String.format("%s%s-%s-%s.%s.%s",
//                    prefix,
//                    c.getBaseName(),
//                    c.getVersion(),
//                    c.getRelease(),
//                    c.getArchitecture(),
//                    c.getExtension());
        }

        return path;
    }

    /**
     * @param path The filename of the RPM-package.
     * @return Returns a RpmArtifactCoordinates object with all included  coordinates set
     */
    public static RpmCoordinates parse(String path)
    {
        return RpmArtifactCoordinatesUtils.parse(path);
    }
    public static String calculatePackageId(String packageScope, String packageName)
    {
        //TODO 待实现
        return packageScope == null ? packageName : String.format("%s/%s", packageScope, packageName);
    }

    //public static RpmArtifactCoordinates of(String packageId,
    //                                        String version)
    //{
    //    if (packageId.contains("/"))
    //    {
    //        String[] nameSplit = packageId.split("/");
    //
    //        return new RpmArtifactCoordinates(nameSplit[0], nameSplit[1], version, RpmPackageType.SOURCE);
    //    }
    //
    //    return new RpmArtifactCoordinates(null, packageId, version, RpmPackageType.BINARY);
    //}

    public static RpmCoordinates of(String packageId)
    {
        // TODO 待实现
        //if (packageId.contains("/"))
        //{
        //    String[] nameSplit = packageId.split("/");
        //
        //    return RpmArtifactCoordinatesUtils.parse(packageId);
        //}

        return RpmArtifactCoordinatesUtils.parse(packageId);
    }

    public String getName() {
        return "test";

    }

    public String getScope() {
        return "dev";
    }

    public void setPath(String path) {
        this.path = path;
    }

    @Override
    public String getPath() {
        return path;
    }
}
