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
package com.folib.domain;

import java.net.URL;
import java.util.Date;
import java.util.Objects;

public class FileContent
{
    private String name;

    private Long size;

    private Date lastModified;

    private String storageId;

    private String repositoryId;

    private String artifactPath;

    private String layout;

    private URL url;

    private String path;

    private String description;

    public FileContent()
    {

    }

    public FileContent(String name)
    {
        setName(name);
    }

    public String getName()
    {
        return name;
    }

    public Long getSize()
    {
        return size;
    }

    public Date getLastModified()
    {
        return lastModified;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public String getArtifactPath()
    {
        return artifactPath;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public void setSize(Long size)
    {
        this.size = size;
    }

    public void setLastModified(Date lastModified)
    {
        this.lastModified = lastModified;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public void setArtifactPath(String artifactPath)
    {
        this.artifactPath = artifactPath;
    }

    public URL getUrl()
    {
        return url;
    }

    public void setUrl(URL url)
    {
        this.url = url;
    }

    public String getLayout() {
        return layout;
    }

    public void setLayout(String layout) {
        this.layout = layout;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof FileContent)){
            return false;
        }
        FileContent that = (FileContent) o;
        return artifactPath.equals(that.artifactPath);
    }

    @Override
    public int hashCode() {
        return Objects.hash(artifactPath);
    }
}
