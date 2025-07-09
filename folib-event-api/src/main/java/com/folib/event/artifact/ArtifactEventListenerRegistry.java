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

import com.folib.event.AbstractEventListenerRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.nio.file.Path;

/**
 * @author Veadan
 */
@Component
public class ArtifactEventListenerRegistry extends AbstractEventListenerRegistry {

    private static final Logger logger = LoggerFactory.getLogger(ArtifactEventListenerRegistry.class);

    public void dispatchArtifactUploadingEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPLOADING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPLOADING event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactUpdatedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactMetadataStoredEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_STORED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_STORED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactDownloadingEvent(Path path)
    {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactDownloadedEvent(Path path)
    {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactCacheEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_CACHE.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_CACHE event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactCopyingEvent(Path srcPath,
                                             Path dstPath) {
        ArtifactEvent event = new ArtifactEvent(srcPath,
                dstPath,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_COPYING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_COPYING event for {} to {}...", srcPath, dstPath);

        dispatchEvent(event);
    }

    public void dispatchArtifactCopiedEvent(Path srcPath,
                                            Path dstPath) {
        ArtifactEvent event = new ArtifactEvent(srcPath,
                dstPath,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_COPIED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_COPIED event for {} to {}...", srcPath, dstPath);

        dispatchEvent(event);
    }

    public void dispatchArtifactMovingEvent(Path srcPath,
                                            Path dstPath) {
        ArtifactEvent event = new ArtifactEvent(srcPath,
                dstPath,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_MOVING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_MOVING event for {} to {}...", srcPath, dstPath);

        dispatchEvent(event);
    }

    public void dispatchArtifactMovedEvent(Path srcPath,
                                           Path dstPath) {
        ArtifactEvent event = new ArtifactEvent(srcPath,
                dstPath,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_MOVED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_MOVED event for {} to {}...", srcPath, dstPath);

        dispatchEvent(event);
    }

    public void dispatchArtifactPathDeletedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactDirectoryPathDeletedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactArchivingEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_ARCHIVING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_MOVED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactArchivedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_ARCHIVED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_ARCHIVED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactMetadataDownloadedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_DOWNLOADED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_DOWNLOADED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactMetadataDownloadingEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_DOWNLOADING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_DOWNLOADING event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactChecksumDownloadedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_CHECKSUM_DOWNLOADED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_CHECKSUM_DOWNLOADED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactChecksumDownloadingEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_CHECKSUM_DOWNLOADING.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_CHECKSUM_DOWNLOADING event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactFetchedFromRemoteEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_FETCHED_FROM_REMOTE.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_FETCHED_FROM_REMOTE event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactStoredEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactMetaDataEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE.getType());
        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE event for {}...", path);
        dispatchEvent(event);
    }


    public void dispatchArtifactDownloadBlockedEvent(Path path) {
        ArtifactEvent event = new ArtifactEvent(path,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOAD_BLOCKED.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOAD_BLOCKED event for {}...", path);

        dispatchEvent(event);
    }

    public void dispatchArtifactFilePromotionEvent(String sourceStorageId,
                                                  String sourceRepositoryId,
                                                  String sourcePath,
                                                  String targetStorageId,
                                                  String targetRepositoryId,
                                                  String syncNo,
                                                  String targetUrl,
                                                  int syncStatus) {
          PromoteDispenseEvent event = new PromoteDispenseEvent(sourceStorageId,
                sourceRepositoryId,
                sourcePath,
                targetStorageId,
                targetRepositoryId,
                syncNo,
                syncStatus,
                targetUrl,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_PROMOTION.getType());
        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_PROMOTION event for {}...", syncNo);

        dispatchEvent(event);
    }

    public void dispatchArtifactFileDispenseEvent(String sourceStorageId,
                                                  String sourceRepositoryId,
                                                  String sourcePath,
                                                  String targetStorageId,
                                                  String targetRepositoryId,
                                                  String syncNo,
                                                  String targetUrl,
                                                  int syncStatus) {
        PromoteDispenseEvent event = new PromoteDispenseEvent(sourceStorageId,
                sourceRepositoryId,
                sourcePath,
                targetStorageId,
                targetRepositoryId,
                syncNo,
                syncStatus,
                targetUrl,
                ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DISPENSE.getType());

        logger.info("Dispatching ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DISPENSE event for {}...", syncNo);

        dispatchEvent(event);
    }

}
