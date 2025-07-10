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
package com.folib.providers.repository;

import com.folib.artifact.AsyncArtifactEntryHandler;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactArchiveListing;
import com.folib.domain.ArtifactEntity;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.storage.repository.Repository;
import com.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@Slf4j
public class ArtifactUpdatedEventHandler extends AsyncArtifactEntryHandler {

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    public ArtifactUpdatedEventHandler() {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException {
        long size = Files.size(repositoryPath);

        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        if (artifactEntry == null) {
            log.debug("No [{}] for [{}].",
                    Artifact.class.getSimpleName(),
                    repositoryPath);

            return null;
        }
        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
        updateArtifactEntry.setLastUpdated(LocalDateTimeInstance.now());
        updateArtifactEntry.setLastUsed(updateArtifactEntry.getLastUpdated());
        updateArtifactEntry.setSizeInBytes(size);
        try {
            Repository repository = repositoryPath.getRepository();
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
            Set<String> archiveFilenames = layoutProvider.listArchiveFilenames(repositoryPath);
            if (CollectionUtils.isNotEmpty(archiveFilenames)) {
                if (archiveFilenames.size() > 5) {
                    archiveFilenames = archiveFilenames.stream().limit(100).collect(Collectors.toSet());
                }
                ArtifactArchiveListing artifactArchiveListing = updateArtifactEntry.getArtifactArchiveListing();
                artifactArchiveListing.setFilenames(archiveFilenames);
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return updateArtifactEntry;
    }

}
