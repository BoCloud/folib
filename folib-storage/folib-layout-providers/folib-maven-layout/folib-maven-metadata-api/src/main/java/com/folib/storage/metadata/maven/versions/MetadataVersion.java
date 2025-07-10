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
package com.folib.storage.metadata.maven.versions;

import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.repository.metadata.SnapshotVersion;

/**
 * @author Steve Todorov <s.todorov@itnews-bg.com>
 */
public class MetadataVersion implements Comparable<MetadataVersion>
{

    private String version;

    private FileTime createdDate;

    private List<SnapshotVersion> snapshots = new ArrayList<>();

    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

    public FileTime getCreatedDate()
    {
        return createdDate;
    }

    public void setCreatedDate(FileTime createdDate)
    {
        this.createdDate = createdDate;
    }

    public List<SnapshotVersion> getSnapshots()
    {
        return snapshots;
    }

    public void setSnapshots(List<SnapshotVersion> snapshots)
    {
        this.snapshots = snapshots;
    }

    @Override
    public int compareTo(MetadataVersion v1)
    {
        int diff = v1.getVersion().compareTo(this.getVersion());
        if (diff < 1 && this.createdDate.toMillis() > v1.getCreatedDate().toMillis())
        {
            diff = 1;
        }

        return diff;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        MetadataVersion that = (MetadataVersion) o;

        if (version != null ? !version.equals(that.version) : that.version != null)
        {
            return false;
        }
        if (createdDate != null ? !createdDate.equals(that.createdDate) : that.createdDate != null)
        {
            return false;
        }

        return snapshots != null ? snapshots.equals(that.snapshots) : that.snapshots == null;
    }

    @Override
    public int hashCode()
    {
        int result = version != null ? version.hashCode() : 0;
        result = 31 * result + (createdDate != null ? createdDate.hashCode() : 0);
        result = 31 * result + (snapshots != null ? snapshots.hashCode() : 0);

        return result;
    }

}
