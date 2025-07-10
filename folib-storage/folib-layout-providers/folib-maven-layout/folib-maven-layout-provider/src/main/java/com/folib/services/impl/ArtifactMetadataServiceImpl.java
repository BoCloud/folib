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
package com.folib.services.impl;

import com.folib.artifact.locator.ArtifactDirectoryLocator;
import com.folib.configuration.ConfigurationManager;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.services.ArtifactMetadataService;
import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.configuration.Configuration;
import com.folib.locator.handlers.GenerateMavenMetadataOperation;
import com.folib.providers.ProviderImplementationException;
import com.folib.storage.Storage;
import com.folib.storage.metadata.MavenMetadataManager;
import com.folib.storage.metadata.MetadataHelper;
import com.folib.storage.metadata.MetadataType;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;
import java.util.List;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author stodorov
 * @author veadan
 */
@Component
public class ArtifactMetadataServiceImpl
        implements ArtifactMetadataService
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactMetadataServiceImpl.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private MavenMetadataManager mavenMetadataManager;

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    public ArtifactMetadataServiceImpl()
    {
    }

    @Override
    public Metadata getMetadata(String storageId,
                                String repositoryId,
                                String artifactPath)
            throws IOException,
                   XmlPullParserException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RepositoryPath artifactBasePath = repositoryPathResolver.resolve(repository, artifactPath);

        return mavenMetadataManager.readMetadata(artifactBasePath);
    }

    @Override
    public Metadata getMetadata(InputStream is)
            throws IOException, XmlPullParserException
    {
        return mavenMetadataManager.readMetadata(is);
    }

    @Override
    public void rebuildMetadata(String storageId,
                                String basePath)
            throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);

        for (Repository repository : storage.getRepositories().values())
        {
            rebuildMetadata(storageId, repository.getId(), basePath);
        }
    }

    @Override
    public void rebuildMetadata(String storageId,
                                String repositoryId,
                                String basePath)
            throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout()))
        {
            logger.warn("Trying to rebuild metadata of repository {} with unsupported layout {} ", repository.getId(),
                        repository.getLayout());
            return;
        }

        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(repository);
        if (basePath != null && basePath.trim().length() > 0)
        {
            repositoryBasePath = repositoryBasePath.resolve(basePath);
        }

        GenerateMavenMetadataOperation operation = new GenerateMavenMetadataOperation(mavenMetadataManager, artifactEventListenerRegistry);
        operation.setBasePath(repositoryBasePath);

        ArtifactDirectoryLocator locator = new ArtifactDirectoryLocator();
        locator.setOperation(operation);
        locator.locateArtifactDirectories();
    }

    @Override
    public void mergeMetadata(MavenArtifact artifact,
                              Metadata mergeMetadata)
            throws IOException,
                   XmlPullParserException,
                   NoSuchAlgorithmException,
                   ProviderImplementationException
    {
        mavenMetadataManager.mergeAndStore(artifact, mergeMetadata);
    }

    @Override
    public void addVersion(String storageId,
                           String repositoryId,
                           String artifactPath,
                           String version,
                           MetadataType metadataType)
            throws IOException,
                   XmlPullParserException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RepositoryPath artifactBasePath = repositoryPathResolver.resolve(repository, artifactPath);

        Metadata metadata = mavenMetadataManager.readMetadata(artifactBasePath);

        addVersion(metadata, version);

        mavenMetadataManager.storeMetadata(artifactBasePath, version, metadata, metadataType);
    }

    @Override
    public void addVersion(Metadata metadata,
                           String version)
    {
        if (!metadata.getVersioning().getVersions().contains(version))
        {
            metadata.getVersioning().getVersions().add(version);

            // Update the latest field
            MetadataHelper.setLatest(metadata, version);
            // Update the release field
            MetadataHelper.setRelease(metadata, version);
            // Update the lastUpdated field
            MetadataHelper.setLastUpdated(metadata.getVersioning());
        }
        else
        {
            // No need to throw an exception here.
            // Logging the error should suffice.
            logger.error("Version {} already exists in the metadata file.", version);
        }
    }

    @Override
    public void addTimestampedSnapshotVersion(String storageId,
                                              String repositoryId,
                                              String artifactPath,
                                              String version,
                                              String classifier,
                                              String extension)
            throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        String snapshot = ArtifactUtils.toSnapshotVersion(version);

        RepositoryPath artifactBasePath = repositoryPathResolver.resolve(repository, artifactPath);

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(artifactBasePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        Metadata snapshotMetadata = mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId,
                                                                                            artifactBasePath,
                                                                                            snapshot,
                                                                                            false);

        addTimestampedSnapshotVersion(snapshotMetadata, version, classifier, extension);

        mavenMetadataManager.storeMetadata(artifactBasePath.getParent(),
                                           snapshot,
                                           snapshotMetadata,
                                           MetadataType.SNAPSHOT_VERSION_LEVEL);
    }

    @Override
    public void generateLastTimestampedSnapshotVersion(String storageId, String repositoryId, String artifactPath, String version) throws IOException {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        String snapshot = ArtifactUtils.toSnapshotVersion(version);
        RepositoryPath artifactBasePath = repositoryPathResolver.resolve(repository, artifactPath);
        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(artifactBasePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();
        mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId,
                artifactBasePath,
                snapshot,
                true);
    }

    @Override
    public void addTimestampedSnapshotVersion(Metadata metadata,
                                              String version,
                                              String classifier,
                                              String extension)
    {
        List<SnapshotVersion> snapshotVersions = metadata.getVersioning().getSnapshotVersions();

        SnapshotVersion snapshotVersion = MetadataHelper.createSnapshotVersion(metadata.getGroupId(),
                                                                               metadata.getArtifactId(),
                                                                               version,
                                                                               classifier,
                                                                               extension);

        snapshotVersions.add(snapshotVersion);

        // Set the snapshot mapping fields (timestamp + buildNumber)
        MetadataHelper.setupSnapshotVersioning(metadata.getVersioning());

        // Update the lastUpdated field
        MetadataHelper.setLastUpdated(metadata.getVersioning());
    }

    @Override
    public void removeVersion(String storageId,
                              String repositoryId,
                              String artifactPath,
                              String version,
                              MetadataType metadataType)
            throws IOException, XmlPullParserException, NoSuchAlgorithmException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);

        Metadata metadata =  mavenMetadataManager.readMetadata(repositoryPath);
        
        Versioning versioning = metadata.getVersioning();

        if (ArtifactUtils.isSnapshot(version))
        {
            RepositoryPath snapshotRepositoryPath = repositoryPath.resolve(ArtifactUtils.toSnapshotVersion(version));

            Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(repositoryPath);
            String artifactGroupId = artifactGroup.getValue0();
            String artifactId = artifactGroup.getValue1();

            mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId, snapshotRepositoryPath,
                                                                    version, true);
        }

        // Update the latest field
        MetadataHelper.setLatest(metadata, version);
        // Update the release field
        MetadataHelper.setRelease(metadata, version);
        // Update the lastUpdated field
        MetadataHelper.setLastUpdated(metadata.getVersioning());

        // Remove the version
        versioning.removeVersion(version);

        mavenMetadataManager.storeMetadata(repositoryPath, version, metadata, metadataType);
    }

    @Override
    public void removeTimestampedSnapshotVersion(String storageId,
                                                 String repositoryId,
                                                 String artifactPath,
                                                 String version,
                                                 String classifier)
            throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        String snapshot = ArtifactUtils.toSnapshotVersion(version);

        RepositoryPath artifactBasePath = repositoryPathResolver.resolve(repository, artifactPath);

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(artifactBasePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        Metadata snapshotMetadata = mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId, artifactId,
                                                                                            artifactBasePath,
                                                                                            snapshot,
                                                                                            false);

        List<SnapshotVersion> snapshotVersions = snapshotMetadata.getVersioning().getSnapshotVersions();
        for (Iterator<SnapshotVersion> iterator = snapshotVersions.iterator(); iterator.hasNext(); )
        {
            SnapshotVersion snapshotVersion = iterator.next();
            if (snapshotVersion.getVersion().equals(version) &&
                (classifier == null || snapshotVersion.getClassifier().equals(classifier)))
            {
                iterator.remove();

                logger.info("Removed timestamped SNAPSHOT ({}{}) from metadata.",
                             version,
                             (classifier != null ? ":" + classifier :
                              (snapshotVersion.getClassifier() != null && !snapshotVersion.getClassifier().equals("") ?
                               ":" + snapshotVersion.getClassifier() + ":" : ":") +
                              snapshotVersion.getExtension()));
            }
        }

        // Set the snapshot mapping fields (timestamp + buildNumber)
        MetadataHelper.setupSnapshotVersioning(snapshotMetadata.getVersioning());

        // Update the lastUpdated field
        MetadataHelper.setLastUpdated(snapshotMetadata.getVersioning());

        mavenMetadataManager.storeMetadata(artifactBasePath,
                                           snapshot,
                                           snapshotMetadata,
                                           MetadataType.SNAPSHOT_VERSION_LEVEL);
    }

    @Override
    public void deleteMetadata(String storageId,
                               String repositoryId,
                               String metadataPath)
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
        if (!Files.isDirectory(repositoryPath))
        {
            return;
        }

        try
        {
            String version = repositoryPath.getFileName().toString();
            RepositoryPath path = repositoryPath.getParent();

            Metadata metadata = mavenMetadataManager.readMetadata(path);
            if (metadata != null && metadata.getVersioning() != null &&
                metadata.getVersioning().getVersions().contains(version))
            {
                metadata.getVersioning().getVersions().remove(version);
                mavenMetadataManager.storeMetadata(path, null, metadata, MetadataType.ARTIFACT_ROOT_LEVEL);
            }
        }
        catch (IOException | XmlPullParserException e)
        {
            // We won't do anything in this case because it doesn't have an impact to the deletion
        }
    }

    public Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

}
