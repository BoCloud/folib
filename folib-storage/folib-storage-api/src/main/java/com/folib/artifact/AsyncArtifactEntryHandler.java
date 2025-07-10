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

import com.folib.domain.Artifact;
import com.folib.event.AsyncEventListener;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathLock;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactService;
import lombok.extern.slf4j.Slf4j;
import org.janusgraph.core.JanusGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;

@Slf4j
public abstract class AsyncArtifactEntryHandler {

    private static final Logger logger = LoggerFactory.getLogger(AsyncArtifactEntryHandler.class);

    @Inject
    private ArtifactRepository artifactEntityRepository;
    @Inject
    private ArtifactService artifactService;
    @Inject
    private RepositoryPathLock repositoryPathLock;
    @Inject
    private JanusGraph janusGraph;
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    private final ArtifactEventTypeEnum eventType;

    public AsyncArtifactEntryHandler(ArtifactEventTypeEnum eventType) {
        this.eventType = eventType;
    }

    @AsyncEventListener
    public void handleEvent(final ArtifactEvent<RepositoryPath> event)
            throws IOException,
            InterruptedException {
        if (eventType.getType() != event.getType()) {
            return;
        }
        RepositoryPath repositoryPath = (RepositoryPath) event.getPath();
        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            return;
        }
        try {
            handleLocked(repositoryPath);
        } catch (Throwable e) {
            logger.error("Failed to handle async event [{}] for [{}]",
                    AsyncArtifactEntryHandler.this.getClass().getSimpleName(),
                    repositoryPath,
                    e);
        }
    }

    private void handleLocked(RepositoryPath repositoryPath)
            throws IOException,
            InterruptedException {
        if (repositoryPathLock.lock(repositoryPath)) {
            try {
                handleTransactional(repositoryPath);
            } finally {
                repositoryPathLock.unLock(repositoryPath);
            }
        } else {
            logger.warn("RepositoryPath [{}] was not get lock", repositoryPath);
        }
    }

    private void handleTransactional(RepositoryPath repositoryPath) {
        try {
            Artifact result = handleEvent(repositoryPath);
            artifactService.saveOrUpdateArtifact(result);
        } catch (Throwable ex) {
            throw new UndeclaredThrowableException(ex);
        }
    }

    protected abstract Artifact handleEvent(RepositoryPath repositoryPath)
            throws IOException;

}
