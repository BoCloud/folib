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

import java.net.URI;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.neo4j.ogm.annotation.NodeEntity;
import org.springframework.util.Assert;

/**
 * @author @author veadan
 *
 */
@NodeEntity(Vertices.NUGET_COORDINATES)
@XmlRootElement(name = "NugetCoordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = NugetCoordinates.LAYOUT_NAME, alias = NugetCoordinates.LAYOUT_ALIAS)
public class NugetCoordinates
        extends LayoutCoordinatesEntity<NugetCoordinates, SemanticVersion>
{

    public static final String LAYOUT_NAME = "NuGet";

    public static final String LAYOUT_ALIAS = "nuget";
    
    public static final String ID = "id";

    public static final String VERSION = "version";

    public static final String EXTENSION = "extension";

    private static final String DEFAULT_EXTENSION = "nupkg";

    private static final String NUGET_PACKAGE_REGEXP_PATTERN = "([a-zA-Z0-9_.-]+)/([a-zA-Z0-9_.-]+)/([a-zA-Z0-9_.-]+).(nupkg|nuspec|nupkg\\.sha512)";

    private static final Pattern NUGET_PACKAGE_REGEXP = Pattern.compile(NUGET_PACKAGE_REGEXP_PATTERN);
    
    public NugetCoordinates()
    {
        resetCoordinates(ID, VERSION, EXTENSION);
    }

    public NugetCoordinates(String id,
                            String version)
    {
        this(id, version, DEFAULT_EXTENSION);
    }

    public NugetCoordinates(String id,
                            String version,
                            String type)
    {
        this();
        setId(id);
        setVersion(version);
        setType(type);
    }

    @Override
    @XmlAttribute(name="id")
    @ArtifactLayoutCoordinate
    public String getId()
    {
        return getCoordinate(ID);
    }

    public void setId(String id)
    {
        setCoordinate(ID, id);
    }

    @Override
    @XmlAttribute(name="version")
    public String getVersion()
    {
        return super.getVersion();
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name="type")
    public String getType()
    {
        return getCoordinate(EXTENSION);
    }

    public void setType(String type)
    {
        setCoordinate(EXTENSION, type);
    }
    
    @Override
    public String convertToPath(NugetCoordinates c)
    {
        String idLocal = c.getId();
        String versionLocal = c.getVersion();
        String typeLocal = c.getType();

        if ("nuspec".equals(typeLocal))
        {
            return String.format("%s/%s/%s.%s", idLocal, versionLocal, idLocal, typeLocal);
        }

        return String.format("%s/%s/%s.%s.%s", idLocal, versionLocal, idLocal, versionLocal, typeLocal);
    }

    @Override
    public URI convertToResource(NugetCoordinates c)
    {
        return URI.create("package/" + c.getId() + "/" + c.getVersion());
    }
    
    @Override
    public SemanticVersion getNativeVersion()
    {
        String versionLocal = getVersion();
        if (versionLocal == null)
        {
            return null;
        }
        try
        {
            return SemanticVersion.parse(versionLocal);
        }
        catch (IllegalArgumentException e)
        {
            return null;
        }
    }
    
    public static NugetCoordinates parse(String path)
    {
        Matcher matcher = NUGET_PACKAGE_REGEXP.matcher(path);

        Assert.isTrue(matcher.matches(), String.format("Illegal artifact path [%s].", path));
        
        String packageId = matcher.group(1);
        String version = matcher.group(2);
        String packageArtifactName = matcher.group(3);
        String packageArtifactType = matcher.group(4);

        Assert.isTrue(String.format("%s.%s", packageId, version).startsWith(packageArtifactName),
                      String.format("Illegal artifact path [%s].", path));

        return new NugetCoordinates(packageId, version, packageArtifactType);
    }
    
}
