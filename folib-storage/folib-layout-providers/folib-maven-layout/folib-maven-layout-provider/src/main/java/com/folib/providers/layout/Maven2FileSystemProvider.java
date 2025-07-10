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

import com.folib.artifact.MavenArtifact;
import com.folib.providers.io.*;
import com.folib.providers.search.SearchException;
import com.folib.services.ArtifactSearchService;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResult;
import com.folib.storage.search.SearchResults;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.storage.Storage;
import com.folib.storage.metadata.MavenMetadataManager;
import com.folib.storage.metadata.MetadataHelper;
import com.folib.storage.metadata.MetadataType;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.io.FilenameUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 *
 */
public class Maven2FileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(Maven2FileSystemProvider.class);
    
    @Inject
    private Maven2LayoutProvider layoutProvider;

    @Inject
    private ArtifactSearchService artifactSearchService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private MavenMetadataManager mavenMetadataManager;
    
    
    public Maven2FileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

    @Override
    public void delete(Path path,
                       boolean force)
        throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) path;
        
        logger.info("Removing {}...", repositoryPath);

//        if (Files.isDirectory(repositoryPath)) {
//            cleanupDirectory(repositoryPath.relativize(), force);
//        }

        super.delete(repositoryPath, force);
    }

    private void cleanupDirectory(RepositoryPath repositoryPathRelative,
                                  boolean force)
        throws IOException
    {
        Repository repository = repositoryPathRelative.getRepository();
        Storage storage = repository.getStorage();
        
        List<String> artifactCoordinateElements = StreamSupport.stream(repositoryPathRelative.spliterator(), false)
                                                               .map(p -> p.toString())
                                                               .collect(Collectors.toList());
        if (artifactCoordinateElements.size() < 2)
        {
            return;
        }
        
        StringBuffer groupId = new StringBuffer();
        for (int i = 0; i < artifactCoordinateElements.size() - 2; i++)
        {
            String element = artifactCoordinateElements.get(i);
            groupId.append((groupId.length() == 0) ? element : "." + element);
        }

        String artifactId = artifactCoordinateElements.get(artifactCoordinateElements.size() - 2);
        String version = artifactCoordinateElements.get(artifactCoordinateElements.size() - 1);

        RepositoryPath pomFilePath = repositoryPathRelative.resolve(artifactId + "-" + version + ".pom");

        // If there is a pom file, read it.
        if (Files.exists(pomFilePath))
        {
            // Run a search against the index and get a list of all the artifacts matching this exact GAV
            SearchRequest request = new SearchRequest(storage.getId(),
                                                      repository.getId(),
                                                      "+g:" + groupId + " " +
                                                      "+a:" + artifactId + " " +
                                                      "+v:" + version);

            try
            {
                SearchResults results = artifactSearchService.search(request);

                for (SearchResult result : results.getResults())
                {
                    delete(repositoryPathResolver.resolve(repository, result.getArtifactCoordinates()), force);
                }
            }
            catch (SearchException e)
            {
                logger.error(e.getMessage(), e);
            }
        }
        // Otherwise, this is either not an artifact directory, or not a valid Maven artifact
    }

    @Override
    public void deleteMetadata(RepositoryPath artifactPath)
    {
        try
        {
            RepositoryPath artifactBasePath = artifactPath;
            RepositoryPath artifactIdLevelPath;
            try
            {
                artifactIdLevelPath = artifactBasePath.getParent();
            }
            catch (RepositoryRelativePathConstructionException e)
            {
                //it's repository root directory, so we have nothing to clean here
                return;
            }

            if (Files.exists(artifactPath))
            {

                RepositoryFileAttributes artifactFileAttributes = Files.readAttributes(artifactPath,
                                                                                       RepositoryFileAttributes.class);

                if (!artifactFileAttributes.isDirectory())
                {
                    artifactBasePath = artifactBasePath.getParent();
                    artifactIdLevelPath = artifactIdLevelPath.getParent();

                    // This is at the version level
                    try (Stream<Path> pathStream = Files.list(artifactBasePath))
                    {
                        Path pomPath = pathStream.filter(p -> p.getFileName()
                                                 .toString()
                                                 .endsWith(".pom"))
                                                 .findFirst()
                                                 .orElse(null);

                        if (pomPath != null)
                        {
                            MavenArtifact mavenArtifact = MavenArtifactUtils
                                    .convertPathToArtifact(RepositoryFiles.relativizePath(artifactPath));
                            if (Objects.nonNull(mavenArtifact)) {
                                String version = mavenArtifact.getVersion();
                                version = version == null ? pomPath.getParent().getFileName().toString() : version;
                                deleteMetadataAtArtifactLevel(artifactBasePath, version);
                            }
                        }
                    }

                }
            }
            else
            {
                artifactBasePath = artifactBasePath.getParent();
                if (artifactBasePath.getTarget().toString().startsWith("s3://")) {
                    Files.delete(artifactBasePath.getTarget());
                    return;
                }
                artifactIdLevelPath = artifactIdLevelPath.getParent();
            }

            if (Files.exists(artifactIdLevelPath))
            {
                // This is at the artifact level
                try (Stream<Path> pathStream = Files.list(artifactIdLevelPath))
                {
                    Path mavenMetadataPath = pathStream.filter(p -> p.getFileName()
                                                       .toString()
                                                       .endsWith("maven-metadata.xml"))
                                                       .findFirst()
                                                       .orElse(null);

                    if (mavenMetadataPath != null)
                    {
                        String version = FilenameUtils.getName(artifactBasePath.toString());

                        deleteMetadataAtArtifactLevel((RepositoryPath) mavenMetadataPath.getParent(), version);
                    }
                }
            }
        }
        catch (IOException | XmlPullParserException e)
        {
            // We won't do anything in this case because it doesn't have an impact to the deletion
            logger.error(e.getMessage(), e);
        }
    }

    public void deleteMetadataAtVersionLevel(RepositoryPath metadataBasePath,
                                             String version)
        throws IOException,
               XmlPullParserException
    {
        
        if (ArtifactUtils.isSnapshot(version) && Files.exists(metadataBasePath))
        {
            Metadata metadataVersionLevel = mavenMetadataManager.readMetadata(metadataBasePath);
            if (metadataVersionLevel != null && metadataVersionLevel.getVersioning() != null &&
                    metadataVersionLevel.getVersioning().getVersions().contains(version))
            {
                metadataVersionLevel.getVersioning().getVersions().remove(version);

                MetadataHelper.setLastUpdated(metadataVersionLevel.getVersioning());

                mavenMetadataManager.storeMetadata(metadataBasePath,
                                                   null,
                                                   metadataVersionLevel,
                                                   MetadataType.SNAPSHOT_VERSION_LEVEL);
            }
        }
    }

    public void deleteMetadataAtArtifactLevel(RepositoryPath artifactPath,
                                              String version)
        throws IOException,
               XmlPullParserException
    {

        Metadata metadataVersionLevel = mavenMetadataManager.readMetadata(artifactPath);
        if (metadataVersionLevel != null && metadataVersionLevel.getVersioning() != null)
        {
            if (ArtifactUtils.isSnapshot(version)) {
                metadataVersionLevel.getVersioning().getSnapshotVersions().removeIf(snapshotVersion -> snapshotVersion.getVersion().equals(version));
            } else {
                metadataVersionLevel.getVersioning().getVersions().remove(version);
            }

            if (version.equals(metadataVersionLevel.getVersioning().getLatest()))
            {
                MetadataHelper.setLatest(metadataVersionLevel);
            }

            if (version.equals(metadataVersionLevel.getVersioning().getRelease()))
            {
                MetadataHelper.setRelease(metadataVersionLevel);
            }

            if (version.equals(metadataVersionLevel.getVersion()))
            {
                metadataVersionLevel.setVersion(metadataVersionLevel.getVersioning().getLatest());
            }

            MetadataHelper.setLastUpdated(metadataVersionLevel.getVersioning());

            mavenMetadataManager.storeMetadata(artifactPath,
                                               null,
                                               metadataVersionLevel,
                                               MetadataType.ARTIFACT_ROOT_LEVEL);
        }
    }
}
