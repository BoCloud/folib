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

import java.time.ZoneOffset;
import java.util.Map;

import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.domain.Artifact;
import org.apache.commons.collections4.MapUtils;
import org.apache.maven.index.ArtifactAvailability;
import org.apache.maven.index.ArtifactContext;
import org.apache.maven.index.ArtifactInfo;
import org.apache.maven.index.creator.MinimalArtifactInfoIndexCreator;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@Order(1)
public class ArtifactEntryMinimalArtifactInfoIndexCreator
        extends MinimalArtifactInfoIndexCreator
{

    public static final ArtifactEntryMinimalArtifactInfoIndexCreator INSTANCE = new ArtifactEntryMinimalArtifactInfoIndexCreator();

    private ArtifactEntryMinimalArtifactInfoIndexCreator()
    {
        super();
    }

    public void populateArtifactInfo(ArtifactContext artifactContext)
    {
        ArtifactEntryArtifactContext ac = (ArtifactEntryArtifactContext) artifactContext;
        Artifact artifactEntry = ac.getArtifactEntry();

        MavenCoordinates coordinates = (MavenCoordinates) artifactEntry.getArtifactCoordinates();

        ArtifactInfo ai = ac.getArtifactInfo();

        // TODO handle artifacts without poms
        if (ac.pomExists())
        {
            if (ai.getClassifier() != null)
            {
                ai.setSourcesExists(ArtifactAvailability.NOT_AVAILABLE);

                ai.setJavadocExists(ArtifactAvailability.NOT_AVAILABLE);
            }
            else
            {
                boolean sourcesExists = ac.sourcesExists();
                if (!sourcesExists)
                {
                    ai.setSourcesExists(ArtifactAvailability.NOT_PRESENT);
                }
                else
                {
                    ai.setSourcesExists(ArtifactAvailability.PRESENT);
                }

                boolean javadocExists = ac.javadocExists();
                if (!javadocExists)
                {
                    ai.setJavadocExists(ArtifactAvailability.NOT_PRESENT);
                }
                else
                {
                    ai.setJavadocExists(ArtifactAvailability.PRESENT);
                }
            }
        }

        /* We do not support this. Should we ?
        Model model = ac.getPomModel();

        if (model != null)
        {
            ai.setName(model.getName());

            ai.setDescription(model.getDescription());

            // for main artifacts (without classifier) only:
            if (ai.getClassifier() == null)
            {
                // only when this is not a classified artifact
                if (model.getPackaging() != null)
                {
                    // set the read value that is coming from POM
                    ai.setPackaging(model.getPackaging());
                }
                else
                {
                    // default it, since POM is present, is read, but does not contain explicit packaging
                    // TODO: this change breaks junit tests, but not sure why is "null" expected value?
                    ai.setPackaging("jar");
                }
            }
        }
        */

        populateArtifactInfoBySha1(ai, artifactEntry.getChecksums());

        ai.setLastModified(artifactEntry.getLastUpdated().atZone(ZoneOffset.systemDefault()).toInstant().getEpochSecond());

        ai.setSize(artifactEntry.getSizeInBytes());

        ai.setFileExtension(coordinates.getExtension());
    }

    private void populateArtifactInfoBySha1(ArtifactInfo artifactInfo,
                                            Map<String, String> checksums)
    {
        artifactInfo.setSignatureExists(ArtifactAvailability.NOT_PRESENT);
        if (MapUtils.isEmpty(checksums))
        {
            return;
        }
        String sha1Checksum = checksums.get("SHA-1");
        if (sha1Checksum == null)
        {
            return;
        }
        artifactInfo.setSignatureExists(ArtifactAvailability.PRESENT);
        artifactInfo.setSha1(sha1Checksum);
    }

}
