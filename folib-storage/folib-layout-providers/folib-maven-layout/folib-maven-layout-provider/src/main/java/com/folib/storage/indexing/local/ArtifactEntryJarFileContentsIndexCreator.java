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

import java.util.Set;

import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactArchiveListing;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.index.ArtifactContext;
import org.apache.maven.index.ArtifactInfo;
import org.apache.maven.index.creator.JarFileContentsIndexCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class ArtifactEntryJarFileContentsIndexCreator
        extends JarFileContentsIndexCreator
{

    public static final ArtifactEntryJarFileContentsIndexCreator INSTANCE = new ArtifactEntryJarFileContentsIndexCreator();

    private ArtifactEntryJarFileContentsIndexCreator()
    {
        super();
    }

    private static final Logger logger = LoggerFactory.getLogger(ArtifactEntryJarFileContentsIndexCreator.class);

    @Override
    public void populateArtifactInfo(ArtifactContext artifactContext)
    {
        ArtifactEntryArtifactContext artifactEntryArtifactContext = (ArtifactEntryArtifactContext) artifactContext;
        Artifact artifactEntry = artifactEntryArtifactContext.getArtifactEntry();
        ArtifactInfo artifactInfo = artifactEntryArtifactContext.getArtifactInfo();

        final MavenCoordinates coordinates = (MavenCoordinates) artifactEntry.getArtifactCoordinates();
        final String extension = coordinates.getExtension();

        if ("jar" .equals(extension) ||
            "war" .equals(extension) ||
            "zip" .equals(extension))
        {
            updateArtifactInfo(artifactInfo, artifactEntry);
        }
    }

    /**
     * @see JarFileContentsIndexCreator#updateArtifactInfo(org.apache.maven.index.ArtifactInfo, java.io.File)
     */
    private void updateArtifactInfo(final ArtifactInfo artifactInfo,
                                    final Artifact artifactEntry)
    {
        final MavenCoordinates coordinates = (MavenCoordinates) artifactEntry.getArtifactCoordinates();

        String strippedPrefix = null;
        if ("war" .equals(coordinates.getExtension()))
        {
            strippedPrefix = "WEB-INF/classes/";
        }

        updateArtifactInfo(artifactInfo, artifactEntry, strippedPrefix);
    }

    /**
     * @see org.apache.maven.index.creator.JarFileContentsIndexCreator#updateArtifactInfo(org.apache.maven.index.ArtifactInfo, java.io.File, java.lang.String)
     */
    private void updateArtifactInfo(final ArtifactInfo artifactInfo,
                                    final Artifact artifactEntry,
                                    final String strippedPrefix)
    {
        ArtifactArchiveListing artifactArchiveListing = artifactEntry.getArtifactArchiveListing();
        if (artifactArchiveListing == null || CollectionUtils.isEmpty(artifactArchiveListing.getFilenames()))
        {
            return;
        }

        Set<String> filenames = artifactArchiveListing.getFilenames();

        final StringBuilder sb = new StringBuilder();

        for (final String name : filenames)
        {
            if (name.endsWith(".class"))
            {
                // original maven indexer skips inner classes too
                final int i = name.indexOf("$");

                if (i == -1)
                {
                    if (name.charAt(0) != '/')
                    {
                        sb.append('/');
                    }

                    if (StringUtils.isBlank(strippedPrefix))
                    {
                        // class name without ".class"
                        sb.append(name, 0, name.length() - 6).append('\n');
                    }
                    else if (name.startsWith(strippedPrefix)
                             && (name.length() > (strippedPrefix.length() + 6)))
                    {
                        // class name without ".class" and stripped prefix
                        sb.append(name, strippedPrefix.length(), name.length() - 6).append('\n');
                    }
                }
            }
        }

        final String fieldValue = sb.toString().trim();

        logger.info("Updating ArtifactInfo using artifactEntry [{}] by classNames [{}]",
                     artifactEntry,
                     fieldValue);

        if (fieldValue.length() != 0)
        {
            artifactInfo.setClassNames(fieldValue);
        }
        else
        {
            artifactInfo.setClassNames(null);
        }
    }
}
