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


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.versioning.ComparableVersion;
import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.artifact.MavenRepositoryArtifact;
import com.folib.db.schema.Vertices;
import com.folib.domain.LayoutCoordinatesEntity;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author Veadan
 */
@NodeEntity(Vertices.MAVEN_COORDINATES)
@XmlRootElement(name = "maven-artifact-coordinates")
@XmlAccessorType(XmlAccessType.NONE)
@CoordinatesLayout(name = MavenCoordinates.LAYOUT_NAME, alias = MavenCoordinates.LAYOUT_ALIAS)
public class MavenCoordinates
        extends LayoutCoordinatesEntity<MavenCoordinates, ComparableVersion>
{

    public static final String LAYOUT_NAME = "Maven 2";

    public static final String LAYOUT_ALIAS = "maven";

    private static final String GROUPID = "groupId";

    private static final String ARTIFACTID = "artifactId";

    private static final String VERSION = "version";

    private static final String CLASSIFIER = "classifier";

    private static final String EXTENSION = "extension";

    public MavenCoordinates()
    {
        resetCoordinates(GROUPID, ARTIFACTID, VERSION, CLASSIFIER, EXTENSION);
    }

    public MavenCoordinates(String... coordinateValues)
    {
        this();

        int i = 0;
        for (String coordinateValue : coordinateValues)
        {
            // Please, forgive the following construct...
            // (In my defense, I felt equally stupid and bad for doing it this way):
            switch (i)
            {
                case 0:
                    setGroupId(coordinateValue);
                    break;
                case 1:
                    setArtifactId(coordinateValue);
                    break;
                case 2:
                    setVersion(coordinateValue);
                    break;
                case 3:
                    setClassifier(coordinateValue);
                    break;
                case 4:
                    setExtension(coordinateValue);
                    break;
                default:
                    break;
            }

            i++;
        }

        if (getExtension() == null)
        {
            setExtension("jar");
        }

    }

    public MavenCoordinates(Artifact artifact)
    {
        setGroupId(artifact.getGroupId());
        setArtifactId(artifact.getArtifactId());
        setVersion(artifact.getVersion());
        setClassifier(artifact.getClassifier());

        String type = artifact.getType();
        if (StringUtils.isNotBlank(type))
        {
            setExtension(type);
        }
        else
        {
            setExtension("jar");
        }

    }
    
    @Override
    public String convertToPath(MavenCoordinates c)
    {
        return MavenArtifactUtils.convertArtifactToPath(c.toArtifact());
    }

    public MavenArtifact toArtifact()
    {
        return new MavenRepositoryArtifact(getGroupId(), getArtifactId(), getVersion(), getExtension(), getClassifier());
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "groupId")
    public String getGroupId()
    {
        return getCoordinate(GROUPID);
    }

    public void setGroupId(String groupId)
    {
        setCoordinate(GROUPID, groupId);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "artifactId")
    public String getArtifactId()
    {
        return getCoordinate(ARTIFACTID);
    }

    public void setArtifactId(String artifactId)
    {
        setCoordinate(ARTIFACTID, artifactId);
    }

    @Override
    public String getId()
    {
        return String.format("%s:%s", getGroupId(), getArtifactId());
    }

    public void setId(String id)
    {
        setArtifactId(id);
    }

    @Override
    @XmlAttribute(name = "version")
    public String getVersion()
    {
        return super.getVersion();
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "classifier")
    public String getClassifier()
    {
        return getCoordinate(CLASSIFIER);
    }

    public void setClassifier(String classifier)
    {
        setCoordinate(CLASSIFIER, classifier);
    }

    @ArtifactLayoutCoordinate
    @XmlAttribute(name = "extension")
    public String getExtension()
    {
        return getCoordinate(EXTENSION);
    }

    public void setExtension(String extension)
    {
        setCoordinate(EXTENSION, extension);
    }

    @Override
    public ComparableVersion getNativeVersion()
    {
        String versionLocal = getVersion();
        if (versionLocal == null)
        {
            return null;
        }
        return new ComparableVersion(versionLocal);
    }

    @Override
    public String toString()
    {
        return "MavenArtifactCoordinates{" + "groupId='" + getGroupId() + '\'' + ", artifactId='" + getArtifactId() + '\'' +
               ", version='" + getVersion() + '\'' + ", classifier='" + getClassifier() + '\'' + ", extension='" + getExtension() +
               '\'' + ", as path: " + convertToPath(this) + '}';
    }

}
