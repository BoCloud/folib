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
package com.folib.event.artifact;

import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.client.RestArtifactResolver;
import com.folib.event.AsyncEventListener;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathLock;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.providers.repository.proxied.ProxyRepositoryInputStream;
import com.folib.providers.repository.proxied.RestArtifactResolverFactory;
import com.folib.providers.ProviderImplementationException;
import com.folib.storage.metadata.MetadataHelper;
import com.folib.storage.metadata.MetadataType;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;

import jakarta.inject.Inject;
import java.io.BufferedInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;

import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenArtifactFetchedFromRemoteEventListener
        extends BaseMavenArtifactEventListener
{

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    protected RepositoryPathLock repositoryPathLock;
    
    @Inject
    protected RestArtifactResolverFactory restArtifactResolverFactory;
    
    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event)
    {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout()))
        {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_FETCHED_FROM_REMOTE.getType())
        {
            return;
        }

        resolveArtifactMetadataAtArtifactIdLevel(event);
        updateMetadataInGroupsContainingRepository(event, path -> path.getParent().getParent());
    }

    private void resolveArtifactMetadataAtArtifactIdLevel(final ArtifactEvent<RepositoryPath> event)
    {
        try
        {
            final RepositoryPath artifactAbsolutePath = event.getPath().toAbsolutePath();
            final RepositoryPath artifactBaseAbsolutePath = artifactAbsolutePath.getParent();

            final RepositoryPath metadataAbsolutePath = (RepositoryPath) MetadataHelper.getMetadataPath(
                    artifactBaseAbsolutePath,
                    null,
                    MetadataType.PLUGIN_GROUP_LEVEL);
            try
            {
                mavenMetadataManager.readMetadata(artifactBaseAbsolutePath.getParent());
            }
            catch (final FileNotFoundException ex)
            {
                downloadArtifactMetadataAtArtifactIdLevelFromRemote(metadataAbsolutePath);
                return;
            }

            downloadArtifactMetadataAtArtifactIdLevelFromRemoteAndMergeWithLocal(artifactAbsolutePath,
                                                                                 metadataAbsolutePath);
        }
        catch (Exception e)
        {
            logger.error("Unable to resolve artifact metadata of file {} of repository {}",
                         event.getPath(), getRepository(event).getId(), e);
        }
    }

    private void downloadArtifactMetadataAtArtifactIdLevelFromRemote(RepositoryPath metadataRelativePath)
            throws Exception
    {
        proxyRepositoryArtifactResolver.fetchRemoteResource(metadataRelativePath);
    }

    private void downloadArtifactMetadataAtArtifactIdLevelFromRemoteAndMergeWithLocal(final RepositoryPath artifactAbsolutePath,
                                                                                      final RepositoryPath metadataPath)
            throws Exception
    {
        Repository repository = metadataPath.getRepository();
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        RestArtifactResolver client = restArtifactResolverFactory.newInstance(remoteRepository, artifactAbsolutePath);

        if (repositoryPathLock.lock(metadataPath)) {
            try (InputStream is = new BufferedInputStream(new ProxyRepositoryInputStream(client, metadataPath)))
            {
                mergeMetadata(artifactAbsolutePath, is);
            }
            finally
            {
                repositoryPathLock.unLock(metadataPath);
            }
        }
    }

    private void mergeMetadata(RepositoryPath repositoryPath,
                               InputStream remoteMetadataIs)
        throws IOException,
        NoSuchAlgorithmException,
        XmlPullParserException,
        ProviderImplementationException
    {
        MavenArtifact localArtifact = MavenArtifactUtils.convertPathToArtifact(repositoryPath);
        Metadata metadata = artifactMetadataService.getMetadata(remoteMetadataIs);
        
        artifactMetadataService.mergeMetadata(localArtifact, metadata);

    }

}
