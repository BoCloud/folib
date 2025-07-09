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
package com.folib.repository.group.metadata;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProvider;
import com.folib.repository.group.BaseMavenGroupRepositoryComponent;
import com.folib.storage.metadata.MavenMetadataManager;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;

import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenMetadataGroupRepositoryComponent
        extends BaseMavenGroupRepositoryComponent
{

    @Inject
    private MavenMetadataManager mavenMetadataManager;

    @Override
    protected void cleanupGroupWhenArtifactPathNoLongerExistsInSubTree(final Repository groupRepository,
                                                                       final String artifactPath)
            throws IOException
    {
        final LayoutProvider layoutProvider = getRepositoryProvider(groupRepository);
        
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(groupRepository, artifactPath);
        Files.deleteIfExists(repositoryPath);
    }

    @Override
    protected UpdateCallback newInstance(RepositoryPath repositoryPath)
    {
        return new MetadataUpdateCallback(repositoryPath);
    }

    class MetadataUpdateCallback
            implements UpdateCallback
    {

        private final RepositoryPath initiatorRepositoryPath;

        private Metadata mergeMetadata;

        MetadataUpdateCallback(RepositoryPath repositoryPath)
        {
            this.initiatorRepositoryPath = repositoryPath;
        }

        @Override
        public void beforeUpdate()
                throws IOException
        {
            final RepositoryPath artifactAbsolutePath = initiatorRepositoryPath.toAbsolutePath();

            try
            {
                mergeMetadata = mavenMetadataManager.readMetadata(artifactAbsolutePath);
            }
            catch (final FileNotFoundException ex)
            {
                logger.warn("Unable to read metadata in repository path {}.", artifactAbsolutePath);
                throw new StopUpdateSilentlyException();
            }
            catch (final XmlPullParserException e)
            {
                throw new IOException(e);
            }
        }

        @Override
        public void performUpdate(final RepositoryPath parentRepositoryArtifactAbsolutePath)
                throws IOException
        {
            mavenMetadataManager.mergeAndStore(parentRepositoryArtifactAbsolutePath, mergeMetadata);
        }
    }

}
