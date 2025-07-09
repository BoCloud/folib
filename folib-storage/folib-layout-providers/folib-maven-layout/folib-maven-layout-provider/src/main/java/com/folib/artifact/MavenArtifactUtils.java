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
package com.folib.artifact;

import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.index.artifact.M2GavCalculator;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class MavenArtifactUtils
{

    private static final Logger logger = LoggerFactory.getLogger(MavenArtifactUtils.class);

    private static final M2GavCalculator M2_GAV_CALCULATOR = new M2GavCalculator();

    public static Gav convertArtifactToGav(Artifact artifact)
    {
        return new Gav(artifact.getGroupId(),
                       StringUtils.defaultString(artifact.getArtifactId()),
                       StringUtils.defaultString(artifact.getVersion()),
                       artifact.getClassifier(),
                       artifact.getType(),
                       null,
                       null,
                       null, false,
                       null,
                       false,
                       null);
    }

    public static String convertArtifactToPath(Artifact artifact)
    {
        final Gav gav = convertArtifactToGav(artifact);
        return M2_GAV_CALCULATOR.gavToPath(gav).substring(1);
    }


    public static MavenArtifact convertPathToArtifact(RepositoryPath repositoryPath)
            throws IOException
    {
        final Gav gav = convertPathToGav(repositoryPath);
        return gav != null ? new MavenRepositoryArtifact(gav, repositoryPath) : null;
    }

    public static MavenArtifact convertPathToArtifact(String path)
    {
        final Gav gav = convertPathToGav(path);
        return gav != null ? new MavenRepositoryArtifact(gav) : null;
    }

    public static Gav convertPathToGav(RepositoryPath repositoryPath)
            throws IOException
    {
        final String path = RepositoryFiles.relativizePath(repositoryPath);
        return convertPathToGav(path);
    }

    public static Gav convertPathToGav(String path)
    {
        return M2_GAV_CALCULATOR.pathToGav(path);
    }

    public static boolean isGAV(RepositoryPath repositoryPath)
    {
        try
        {
            return convertPathToGav(repositoryPath) != null;
        }
        catch (Exception ex)
        {
            logger.warn(ex.getMessage(), ex);
        }
        return false;
    }

    /**
     * Creates a pair of groupId and artifactId from the provided {@link RepositoryPath}.
     * <p>
     * artifactId is created from part of the provided path.
     * groupId is created from the rest of the provided path
     *
     * @param directoryPath {@link RepositoryPath} expected to be a directory
     */
    public static Pair<String, String> getDirectoryGA(RepositoryPath directoryPath)
            throws IOException
    {
        String path = RepositoryFiles.relativizePath(directoryPath);
        if (path.endsWith("/"))
        {
            path = StringUtils.substringBeforeLast(path, "/");
        }
        return Pair.with(StringUtils.substringBeforeLast(path, "/").replaceAll("/", "."),
                         StringUtils.substringAfterLast(path, "/"));
    }

}
