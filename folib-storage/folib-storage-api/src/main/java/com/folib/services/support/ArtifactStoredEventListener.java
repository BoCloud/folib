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
package com.folib.services.support;

import com.folib.artifact.AsyncArtifactEntryHandler;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactArchiveListing;
import com.folib.domain.ArtifactEntity;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.storage.repository.Repository;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
public class ArtifactStoredEventListener
        extends AsyncArtifactEntryHandler {

    private static final Logger logger = LoggerFactory.getLogger(ArtifactStoredEventListener.class);

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    public ArtifactStoredEventListener() {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath)
            throws IOException {
        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        if (artifactEntry == null) {
            logger.debug("No [{}] for [{}].",
                    Artifact.class.getSimpleName(),
                    repositoryPath);

            return null;
        }
        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
        Repository repository = repositoryPath.getRepository();
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
        Set<String> archiveFilenames = layoutProvider.listArchiveFilenames(repositoryPath);
        if (CollectionUtils.isNotEmpty(archiveFilenames)) {
            if (archiveFilenames.size() > 5) {
                //TODO: issues/1752
                archiveFilenames = archiveFilenames.stream().limit(100).collect(Collectors.toSet());
            }
            ArtifactArchiveListing artifactArchiveListing = updateArtifactEntry.getArtifactArchiveListing();
            artifactArchiveListing.setFilenames(archiveFilenames);
        }
        return updateArtifactEntry;
    }

}
