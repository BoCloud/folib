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
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.repository.Repository;
import com.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ArtifactDownloadedEventHandler extends AsyncArtifactEntryHandler {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private ArtifactRepository artifactRepository;

    public ArtifactDownloadedEventHandler() {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath) throws IOException {
        return handleEventRecord(repositoryPath, 1L, true);
    }


    public Artifact handleEventRecord(RepositoryPath repositoryPath, Long count, Boolean commit) throws IOException {
        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        if (artifactEntry == null) {
            log.debug("No [{}] for [{}].",
                    Artifact.class.getSimpleName(),
                    repositoryPath);

            return null;
        }
        if (Objects.isNull(count)) {
            count = 1L;
        }
        String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
        RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
             ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
            objectOutputStream.writeObject(artifactEntry);
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            Files.write(artifactRepositoryPath, byteArray);
        } catch (Exception ex) {
            log.debug("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
        }
        Artifact updateArtifactEntry = new ArtifactEntity(artifactEntry.getNativeId(), artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getUuid(), artifactEntry.getArtifactCoordinates());
        Integer downloadCount = artifactEntry.getDownloadCount();
        if (Objects.isNull(downloadCount)) {
            downloadCount = 0;
        }
        updateArtifactEntry.setDownloadCount(downloadCount + count.intValue());
        updateArtifactEntry.setLastUsed(LocalDateTimeInstance.now());
        updateArtifactEntry.setLastUpdated(updateArtifactEntry.getLastUsed());
        log.debug("[{}] [{}] downloadCount changed from [{}] to [{}].",
                this.getClass().getSimpleName(),
                repositoryPath,
                artifactEntry.getDownloadCount(),
                updateArtifactEntry.getDownloadCount()
        );
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
        if (Boolean.TRUE.equals(commit)) {
            artifactRepository.saveOrUpdate(updateArtifactEntry);
        }
        return updateArtifactEntry;
    }

}
