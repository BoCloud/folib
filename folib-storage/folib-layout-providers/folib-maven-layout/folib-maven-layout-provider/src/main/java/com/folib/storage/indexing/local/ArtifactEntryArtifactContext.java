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
package com.folib.storage.indexing.local;

import java.io.File;

import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.domain.Artifact;
import org.apache.commons.io.FilenameUtils;
import org.apache.maven.index.ArtifactContext;
import org.apache.maven.index.ArtifactInfo;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.model.Model;
import com.folib.domain.MavenArtifactEntryUtils;
import org.codehaus.plexus.util.StringUtils;

/**
 * @author veadan
 */
public class ArtifactEntryArtifactContext
        extends ArtifactContext
{

    private final Artifact artifactEntry;
    private final ArtifactEntryArtifactContextHelper artifactEntryArtifactContextHelper;

    public ArtifactEntryArtifactContext(final Artifact artifactEntry,
                                        final ArtifactEntryArtifactContextHelper artifactEntryArtifactContextHelper)
            throws IllegalArgumentException
    {
        super(null, null, null, asArtifactInfo(artifactEntry), asGav(artifactEntry));
        this.artifactEntry = artifactEntry;
        this.artifactEntryArtifactContextHelper = artifactEntryArtifactContextHelper;
    }

    private static ArtifactInfo asArtifactInfo(Artifact artifactEntry)
    {
        final MavenCoordinates coordinates = (MavenCoordinates) artifactEntry.getArtifactCoordinates();
        ArtifactInfo artifactInfo = new ArtifactInfo(artifactEntry.getRepositoryId(),
                                                     coordinates.getGroupId(),
                                                     coordinates.getArtifactId(),
                                                     coordinates.getVersion(),
                                                     coordinates.getClassifier(),
                                                     coordinates.getExtension());

        produce(coordinates, artifactInfo);

        return artifactInfo;
    }

    /**
     * @see org.apache.maven.index.DefaultArtifactContextProducer#getArtifactContext(org.apache.maven.index.context.IndexingContext, java.io.File)
     */
    private static void produce(MavenCoordinates coordinates,
                                ArtifactInfo artifactInfo)
    {
        if (!StringUtils.isEmpty(coordinates.getClassifier()))
        {
            artifactInfo.setPackaging(coordinates.getExtension());
        }

        artifactInfo.setFileName(FilenameUtils.getName(coordinates.buildPath()));
        artifactInfo.setFileExtension(coordinates.getExtension());
    }

    private static Gav asGav(Artifact artifactEntry)
    {
        return MavenArtifactEntryUtils.toGav(artifactEntry);
    }

    public Artifact getArtifactEntry()
    {
        return artifactEntry;
    }

    @Override
    public File getArtifact()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public File getMetadata()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public File getPom()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    @Override
    public Model getPomModel()
    {
        throw new UnsupportedOperationException("This ArtifactContext base on ArtifactEntry");
    }

    public boolean pomExists()
    {
        return artifactEntryArtifactContextHelper.pomExists();
    }

    public boolean sourcesExists()
    {
        return artifactEntryArtifactContextHelper.sourcesExists();
    }

    public boolean javadocExists()
    {
        return artifactEntryArtifactContextHelper.javadocExists();
    }
}
