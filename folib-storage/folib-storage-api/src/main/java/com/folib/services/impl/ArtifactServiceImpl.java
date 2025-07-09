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

import com.folib.domain.*;
import com.google.common.collect.Sets;
import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.components.DistributedLockComponent;
import com.folib.constant.GlobalConstants;
import com.folib.gremlin.dsl.EntityTraversalSource;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CommonUtils;
import com.folib.util.LocalDateTimeInstance;
import com.folib.util.UserUtils;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import jakarta.inject.Inject;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@Transactional
public class ArtifactServiceImpl implements ArtifactService {

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private JanusGraph janusGraph;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private ArtifactTagService artifactTagService;

    @Inject
    @Lazy
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    @Lazy
    private ArtifactIdGroupService artifactIdGroupService;

    @Override
    public void saveOrUpdateArtifact(Artifact artifact) {
        Optional<Artifact> exist = artifactRepository.findById(artifact.getStorageId(),  artifact.getRepositoryId(), artifact.getArtifactPath());

        //Artifact  artifactBase = artifactRepository.findOneArtifactBase(artifact.getStorageId(), artifact.getRepositoryId(),artifact.getArtifactPath());
        ArtifactEntity artifactEntity = (ArtifactEntity)artifact ;
        //if(artifactBase!=null){
        //    artifactEntity.setNativeId(artifactBase.getNativeId());
        //}
        exist.ifPresent(value -> artifactEntity.setNativeId(value.getNativeId()));
        saveOrUpdateArtifact(artifactEntity, true);
    }

    @Override
    public void saveOrUpdateArtifact(Artifact artifact, Boolean immediately) {
        if (distributedLockComponent.lock(artifact.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                Graph g = janusGraph.tx().createThreadedTx();
                try {
                    artifact.setLastUpdated(LocalDateTimeInstance.now());
                    artifact.setLastUsed(artifact.getLastUpdated());
                    artifact.setUpdatedBy(UserUtils.getUsername());
                    artifactRepository.merge(() -> g.traversal(EntityTraversalSource.class), artifact);
                    if (g.tx().isOpen()) {
                        g.tx().commit();
                    }
                    storeArtifactMetadataFile(artifact);
                } catch (Exception ex) {
                    if (g.tx().isOpen()) {
                        g.tx().rollback();
                    }
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle artifact [{}] catch error", artifact.getUuid());
                        return;
                    }
                    log.error("Handle artifact [{}] error [{}]", artifact.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new UndeclaredThrowableException(ex);
                } finally {
                    g.tx().close();
                }
            } finally {
                distributedLockComponent.unLock(artifact.getUuid());
            }
        } else {
            log.warn("Handle artifact [{}] was not get lock", artifact.getUuid());
        }
    }

    @Override
    public Artifact findArtifact(RepositoryPath repositoryPath, Boolean report) throws IOException {
        if (Boolean.FALSE.equals(report)) {
            return artifactRepository.findOneArtifact(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
        }
        return artifactRepository.findArtifactReport(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    public void copyArtifact(RepositoryPath sourceRepositoryPath, RepositoryPath targetRepositoryPath) throws IOException {
        Artifact sourceArtifact = sourceRepositoryPath.getArtifactEntry();
        if (sourceArtifact == null) {
            return;
        }
        Artifact targetArtifact = targetRepositoryPath.getArtifactEntry();
        if (targetArtifact == null) {
            targetArtifact = provideArtifact(targetRepositoryPath);
            if (targetArtifact == null) {
                return;
            }
        }
        targetRepositoryPath.setArtifact(targetArtifact);
        String username = UserUtils.getUsername();
        LocalDateTime now = LocalDateTimeInstance.now();
        if (Objects.isNull(targetArtifact.getNativeId())) {
            targetArtifact.setCreated(now);
            targetArtifact.setCreatedBy(username);
        }
        targetArtifact.setLastUpdated(now);
        targetArtifact.setLastUsed(now);
        targetArtifact.setUpdatedBy(username);
        Repository repository = targetRepositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(targetRepositoryPath);
        targetArtifact.setSizeInBytes(sourceArtifact.getSizeInBytes());
        targetArtifact.setChecksums(sourceArtifact.getChecksums());
        targetArtifact.setMetadata(sourceArtifact.getMetadata());
        targetArtifact.setArtifactFileExists(Boolean.TRUE);
        targetArtifact.setEnabled(Boolean.TRUE);
        ArtifactArchiveListing sourceArtifactArchiveListing = sourceArtifact.getArtifactArchiveListing();
        if (Objects.nonNull(sourceArtifactArchiveListing)) {
            ArtifactArchiveListing artifactArchiveListing = targetArtifact.getArtifactArchiveListing();
            artifactArchiveListing.setFilenames(sourceArtifactArchiveListing.getFilenames());
        }
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactGroupWithTag(storage.getId(),
                repository.getId(),
                coordinates.getId(),
                Optional.of(lastVersionTag))
                .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                        repository.getId(),
                        coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, targetArtifact);
        log.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());
        try {
            artifactIdGroupRepository.saveOrUpdate(artifactGroup);
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            log.warn("[{}] [{}] merge group error [{}]",
                    this.getClass().getSimpleName(), targetRepositoryPath, realMessage);
            if (CommonUtils.catchException(realMessage)) {
                log.warn("[{}] [{}] merge group catch error",
                        this.getClass().getSimpleName(), targetRepositoryPath);
                return;
            }
            throw ex;
        }
    }

    @Override
    public Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException {
        return Optional.ofNullable(repositoryPath.getArtifactEntry())
                .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                        RepositoryFiles.readCoordinates(repositoryPath)));
    }

    /**
     * 存储制品元数据文件
     *
     * @param artifact artifact
     */
    public void storeArtifactMetadataFile(Artifact artifact) {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
            if (Objects.nonNull(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry()) && Files.exists(repositoryPath)) {
                String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
                RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(fileName);
                try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                     ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                    Artifact artifactEntry = repositoryPath.getArtifactEntry();
                    artifactEntry.setMetadata(artifact.getMetadata());
                    objectOutputStream.writeObject(artifactEntry);
                    byte[] byteArray = byteArrayOutputStream.toByteArray();
                    Files.write(artifactRepositoryPath, byteArray);
                } catch (Exception ex) {
                    log.debug("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.warn("StoreArtifactMetadataFile error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }
}
