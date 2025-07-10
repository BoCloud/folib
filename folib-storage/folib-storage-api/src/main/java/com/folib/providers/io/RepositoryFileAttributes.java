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
package com.folib.providers.io;

import com.folib.artifact.coordinates.ArtifactCoordinates;

import java.net.URL;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.HashMap;
import java.util.Map;

public class RepositoryFileAttributes
        implements BasicFileAttributes
{

    private BasicFileAttributes basicAttributes;

    private Map<RepositoryFileAttributeType, Object> attributes = new HashMap<>();

    public RepositoryFileAttributes(BasicFileAttributes basicAttributes)
    {
        super();
        this.basicAttributes = basicAttributes;
    }

    public RepositoryFileAttributes(BasicFileAttributes basicAttributes,
                                    Map<RepositoryFileAttributeType, Object> attributes)
    {
        super();
        this.basicAttributes = basicAttributes;
        this.attributes = attributes;
    }

    public FileTime lastModifiedTime()
    {
        return basicAttributes.lastModifiedTime();
    }

    public FileTime lastAccessTime()
    {
        return basicAttributes.lastAccessTime();
    }

    public FileTime creationTime()
    {
        return basicAttributes.creationTime();
    }

    public boolean isRegularFile()
    {
        return basicAttributes.isRegularFile();
    }

    public boolean isDirectory()
    {
        return basicAttributes.isDirectory();
    }

    public boolean isSymbolicLink()
    {
        return basicAttributes.isSymbolicLink();
    }

    public boolean isOther()
    {
        return basicAttributes.isOther();
    }

    public long size()
    {
        return basicAttributes.size();
    }

    public Object fileKey()
    {
        return basicAttributes.fileKey();
    }

    public ArtifactCoordinates getCoordinates()
    {
        return (ArtifactCoordinates) attributes.get(RepositoryFileAttributeType.COORDINATES);
    }

    protected void setCoordinates(ArtifactCoordinates coordinates)
    {
        attributes.put(RepositoryFileAttributeType.COORDINATES, coordinates);
    }

    public boolean isMetadata()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.METADATA));
    }

    protected void setMetadata(boolean isMetadata)
    {
        attributes.put(RepositoryFileAttributeType.METADATA, isMetadata);
    }

    public boolean isChecksum()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.CHECKSUM));
    }

    protected void setChecksum(boolean isChecksum)
    {
        attributes.put(RepositoryFileAttributeType.CHECKSUM, isChecksum);
    }

    public boolean isTemp()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.TEMP));
    }

    protected void setTemp(boolean isTemp)
    {
        attributes.put(RepositoryFileAttributeType.TEMP, isTemp);
    }

    public boolean isArtifact()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.ARTIFACT));
    }

    protected void setArtifact(boolean isArtifact)
    {
        attributes.put(RepositoryFileAttributeType.ARTIFACT, isArtifact);
    }

    public boolean hasExpired()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.EXPIRED));
    }

    public boolean getResourceUrl()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.RESOURCE_URL));
    }

    protected void setResourceUrl(URL url)
    {
        attributes.put(RepositoryFileAttributeType.RESOURCE_URL, url);
    }

    public boolean getArtifactPath()
    {
        return Boolean.TRUE.equals(attributes.get(RepositoryFileAttributeType.ARTIFACT_PATH));
    }

    protected void setArtifactPath(String path)
    {
        attributes.put(RepositoryFileAttributeType.ARTIFACT_PATH, path);
    }

    public String getStorageId()
    {
        return (String) attributes.get(RepositoryFileAttributeType.STORAGE_ID);
    }

    protected void setStorageId(String id)
    {
        attributes.put(RepositoryFileAttributeType.STORAGE_ID, id);
    }

    public String getRepositoryId()
    {
        return (String) attributes.get(RepositoryFileAttributeType.REPOSITORY_ID);
    }

    public void setRepositoryId(String id)
    {
        attributes.put(RepositoryFileAttributeType.REPOSITORY_ID, id);
    }

}
