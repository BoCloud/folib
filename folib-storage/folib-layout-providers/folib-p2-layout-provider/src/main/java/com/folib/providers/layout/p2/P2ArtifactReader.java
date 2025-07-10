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
package com.folib.providers.layout.p2;

import com.folib.artifact.coordinates.P2Coordinates;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Finds a {@link P2Coordinates} from the provided repository base directory and artifact path.
 * The artifact path consists of {id}/{version}/{classifier}. E.g. bundle-name/1.0.0/osgi.bundle
 */
public class P2ArtifactReader
{

    private static final Logger logger = LoggerFactory.getLogger(P2ArtifactReader.class);

    /**
     * Finds a {@link P2Coordinates} from the provided repository base directory and artifact path.
     *
     * @param repositoryBaseDir The folder containing the repository
     * @param bundle            The artifact path in the format of {id}/{version}/{classifier}. E.g. bundle-name/1.0.0/osgi.bundle
     * @return P2ArtifactCoordinates if found otherwise null
     */
    public static P2Coordinates getArtifact(String repositoryBaseDir,
                                            String bundle)
            throws IOException
    {
        // TODO we are dropping GenericParser and one day we will need to support P2
        //GenericParser<P2Repository> repositoryParser = new GenericParser<>(P2Repository.class);
        P2Repository p2Repository = null;//repositoryParser.parse(createPath(repositoryBaseDir).toUri().toURL());
        final P2Coordinates artifactToFind = P2Coordinates.create(bundle);
        for (P2Artifact p2Artifact : p2Repository.getArtifacts().getArtifacts())
        {
            P2Coordinates foundArtifact = new P2Coordinates(p2Artifact.getId(),
                                                                            p2Artifact.getVersion(),
                                                                            p2Artifact.getClassifier());
            if (foundArtifact.equals(artifactToFind))
            {
                addProperties(foundArtifact, p2Artifact, repositoryBaseDir);
                String bundleFilename = P2ArtifactRuleProcessor.getFilename(p2Repository.getMappings(),
                                                                            foundArtifact);
                foundArtifact.setFilename(bundleFilename);
                return foundArtifact;
            }
        }

        return null;
    }

    private static void addProperties(P2Coordinates foundArtifact,
                                      P2Artifact p2Artifact,
                                      String baseDir)
    {
        foundArtifact.addProperty("repoUrl", baseDir);
        foundArtifact.addProperty("id", p2Artifact.getId());
        foundArtifact.addProperty("version", p2Artifact.getVersion());
        foundArtifact.addProperty("classifier", p2Artifact.getClassifier());
        P2Properties properties = p2Artifact.getProperties();
        if (properties != null)
        {
            properties.getPropertites().forEach(
                    property -> foundArtifact.addProperty(property.getName(), property.getValue()));
        }
    }

    private static Path createPath(String repositoryBaseDir)
    {
        final String artifactsFilename = "artifacts.xml";
        if (repositoryBaseDir == null || repositoryBaseDir.isEmpty())
        {
            return Paths.get(artifactsFilename);
        }

        return Paths.get(repositoryBaseDir).resolve(artifactsFilename);
    }
}
