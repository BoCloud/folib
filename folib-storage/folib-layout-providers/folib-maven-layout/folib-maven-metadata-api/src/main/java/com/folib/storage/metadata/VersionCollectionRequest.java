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
package com.folib.storage.metadata;

import com.folib.storage.metadata.maven.versions.MetadataVersion;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.repository.metadata.Plugin;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;

/**
 * @author veadan
 */
public class VersionCollectionRequest
{

    private Path artifactBasePath;

    private Versioning versioning = new Versioning();

    private List<SnapshotVersion> snapshotVersions = new ArrayList<>();

    private List<MetadataVersion> metadataVersions = new ArrayList<>();

    private List<Plugin> plugins = new ArrayList<>();


    public VersionCollectionRequest()
    {
    }

    public Path getArtifactBasePath()
    {
        return artifactBasePath;
    }

    public void setArtifactBasePath(Path artifactBasePath)
    {
        this.artifactBasePath = artifactBasePath;
    }

    public Versioning getVersioning()
    {
        return versioning;
    }

    public void setVersioning(Versioning versioning)
    {
        this.versioning = versioning;
    }

    public List<SnapshotVersion> getSnapshotVersions()
    {
        return snapshotVersions;
    }

    public void setSnapshotVersions(List<SnapshotVersion> snapshotVersions)
    {
        this.snapshotVersions = snapshotVersions;
    }

    public List<Plugin> getPlugins()
    {
        return plugins;
    }

    public void setPlugins(List<Plugin> plugins)
    {
        this.plugins = plugins;
    }

    public boolean addPlugin(Plugin plugin)
    {
        return plugins.add(plugin);
    }

    public List<MetadataVersion> getMetadataVersions()
    {
        return metadataVersions;
    }

    public void setMetadataVersions(List<MetadataVersion> metadataVersions)
    {
        this.metadataVersions = metadataVersions;
    }

}
