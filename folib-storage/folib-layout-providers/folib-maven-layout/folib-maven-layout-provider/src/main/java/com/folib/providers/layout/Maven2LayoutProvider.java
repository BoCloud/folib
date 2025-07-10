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
package com.folib.providers.layout;


import cn.hutool.crypto.digest.SM3;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.repository.MavenRepositoryStrategy;
import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.artifact.archive.JarArchiveListingFunction;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.storage.metadata.MetadataHelper;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.maven.index.artifact.M2ArtifactRecognizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("maven2LayoutProvider")
public class Maven2LayoutProvider
        extends AbstractLayoutProvider<MavenCoordinates>
{

    public static final String ALIAS = MavenCoordinates.LAYOUT_NAME;

    private static final Logger logger = LoggerFactory.getLogger(Maven2LayoutProvider.class);

    @Lazy
    @Inject
    private MavenRepositoryStrategy mavenRepositoryManagementStrategy;
    @Lazy
    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public MavenCoordinates getArtifactCoordinates(RepositoryPath repositoryPath)
            throws IOException
    {
        MavenArtifact artifact = MavenArtifactUtils.convertPathToArtifact(repositoryPath);

        return new MavenCoordinates(artifact);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512, SM3.ALGORITHM_NAME)
                .collect(Collectors.toSet());
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith(".pom");
    }

    public boolean isMavenMetadata(RepositoryPath path)
    {
        return MetadataHelper.MAVEN_METADATA_XML.equals(path.getFileName().toString());
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException
    {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                                                                                            attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes)
        {
            Object value = result.get(attributeType);
            switch (attributeType)
            {
                case ARTIFACT:
                    value = BooleanUtils.isTrue((Boolean) value) && MavenArtifactUtils.isGAV(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case METADATA:
                    value = BooleanUtils.isTrue((Boolean) value) || isMavenMetadata(repositoryPath);

                    result.put(attributeType, value);

                    break;
                case EXPIRED:
                    final Instant tenSecondsAgo = Instant.now().minus(10, ChronoUnit.SECONDS);
                    value = BooleanUtils.isTrue((Boolean) value) || (isMavenMetadata(repositoryPath)
                                                                     &&
                                                                     !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                                                                                       tenSecondsAgo));

                    result.put(attributeType, value);

                    break;
                default:

                    break;
            }
        }

        return result;
    }

    private boolean isIndex(RepositoryPath path)
    {
        if (!path.isAbsolute())
        {
            return false;
        }
        RepositoryPath indexRoot = path.getFileSystem().getRootDirectory().resolve(MavenRepositoryFeatures.INDEX);
        if (path.startsWith(indexRoot))
        {
            return true;
        }

        return false;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return mavenRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public MavenRepositoryStrategy getRepositoryManagementStrategy()
    {
        return mavenRepositoryManagementStrategy;
    }

    @Override
    public Set<String> listArchiveFilenames(final RepositoryPath repositoryPath)
    {
        if (JarArchiveListingFunction.INSTANCE.supports(repositoryPath))
        {
            try
            {
                return JarArchiveListingFunction.INSTANCE.listFilenames(repositoryPath);
            }
            catch (IOException e)
            {
                logger.warn("Unable to list filenames in archive path {} using {}",
                            repositoryPath, JarArchiveListingFunction.INSTANCE.getClass(), e);
            }
        }
        return Collections.emptySet();
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, String fileName) {
        if (JarArchiveListingFunction.INSTANCE.supports(repositoryPath))
        {
            try
            {
                return JarArchiveListingFunction.INSTANCE.getContentByFileName(repositoryPath, fileName);
            }
            catch (IOException e)
            {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, JarArchiveListingFunction.INSTANCE, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (JarArchiveListingFunction.INSTANCE.supports(repositoryPath))
        {
            try
            {
                return JarArchiveListingFunction.INSTANCE.getContentByFileName(repositoryPath, path, fileName);
            }
            catch (IOException e)
            {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, JarArchiveListingFunction.INSTANCE, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (JarArchiveListingFunction.INSTANCE.supports(repositoryPath))
        {
            try
            {
                return JarArchiveListingFunction.INSTANCE.getContentByEqualsFileName(repositoryPath, path, fileName);
            }
            catch (IOException e)
            {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, JarArchiveListingFunction.INSTANCE, e);
            }
        }
        return null;
    }

    public boolean requiresGroupAggregation(final RepositoryPath repositoryPath)
    {
        return isMavenMetadata(repositoryPath) &&
               !M2ArtifactRecognizer.isSnapshot(repositoryPath.getParent().getFileName().toString());
    }
}
