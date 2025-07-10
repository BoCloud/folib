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
package com.folib.providers.io;

import com.folib.domain.*;
import com.folib.io.*;
import com.google.common.collect.Sets;
import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.data.criteria.Expression.ExpOperator;
import com.folib.data.criteria.Predicate;
import com.folib.data.criteria.Selector;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CommonUtils;
import com.folib.util.LocalDateTimeInstance;
import com.folib.util.RepositoryPathUtil;
import com.folib.util.UserUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.output.CountingOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Lazy;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.util.Assert;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;

/**
 * @author Veadan
 */
public abstract class AbstractRepositoryProvider implements RepositoryProvider, RepositoryStreamCallback {

    private static final Logger logger = LoggerFactory.getLogger(AbstractRepositoryProvider.class);

    @Lazy
    @Inject
    protected RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    protected LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactRepository;

    @Inject
    private ArtifactIdGroupService artifactIdGroupService;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    protected ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    protected ApplicationEventPublisher eventPublisher;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Inject
    private PlatformTransactionManager transactionManager;

    @Inject
    private ArtifactTagService artifactTagService;

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

    @Override
    public RepositoryStreamSupport.RepositoryInputStream getInputStream(Path path)
            throws IOException {
        if (path == null) {
            return null;
        }
        Assert.isInstanceOf(RepositoryPath.class, path);
        RepositoryPath repositoryPath = (RepositoryPath) path;

        return decorate((RepositoryPath) path,
                getInputStreamInternal(repositoryPath));

    }

    @Override
    public RepositoryStreamSupport.RepositoryStoreIndexInputStream getStoreIndexInputStream(Path path)
            throws IOException {
        if (path == null) {
            return null;
        }
        Assert.isInstanceOf(RepositoryPath.class, path);
        RepositoryPath repositoryPath = (RepositoryPath) path;

        return decorateIndex((RepositoryPath) path,
                getInputStreamInternal(repositoryPath));

    }

    protected abstract InputStream getInputStreamInternal(RepositoryPath repositoryPath)
            throws IOException;

    protected RepositoryStreamSupport.RepositoryInputStream decorate(RepositoryPath repositoryPath,
                                                                     InputStream is) throws IOException {
        if (is instanceof RepositoryStreamSupport.RepositoryInputStream) {
            return (RepositoryStreamSupport.RepositoryInputStream) is;
        }

        return new RepositoryStreamSupport(null, this, transactionManager).
                new RepositoryInputStream(repositoryPath, is);
    }

    protected RepositoryStreamSupport.RepositoryStoreIndexInputStream decorateIndex(RepositoryPath repositoryPath,
                                                                                    InputStream is) throws IOException {
        if (is instanceof RepositoryStreamSupport.RepositoryStoreIndexInputStream) {
            return (RepositoryStreamSupport.RepositoryStoreIndexInputStream) is;
        }

        return new RepositoryStreamSupport(repositoryPathLock, this, transactionManager).
                new RepositoryStoreIndexInputStream(repositoryPath, is);
    }

    @Override
    public RepositoryStreamSupport.RepositoryOutputStream getOutputStream(Path path)
            throws IOException {
        Assert.isInstanceOf(RepositoryPath.class, path);
        OutputStream os = getOutputStreamInternal((RepositoryPath) path);

        return decorate((RepositoryPath) path, os);
    }

    protected abstract OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException;

    protected final RepositoryStreamSupport.RepositoryOutputStream decorate(RepositoryPath repositoryPath,
                                                                            OutputStream os) throws IOException {
        if (os == null || os instanceof RepositoryStreamSupport.RepositoryOutputStream) {
            return (RepositoryStreamSupport.RepositoryOutputStream) os;
        }

        return new RepositoryStreamSupport(repositoryPathLock, this, transactionManager).
                new RepositoryOutputStream(repositoryPath, os);
    }

    @Override
    public void onBeforeWrite(RepositoryStreamWriteContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Writing [{}]", repositoryPath);

        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            return;
        }

        Repository repository = repositoryPath.getRepository();
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();

        if (Boolean.TRUE.equals(repositoryPath.getArtifactExist())) {
            return;
        }
        Artifact artifactEntry = repositoryPath.getArtifactEntry();
        if (Objects.isNull(artifactEntry)) {
            artifactEntry = new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                    RepositoryFiles.readCoordinates(repositoryPath));
        }
        artifactEntry.setStorageId(storageId);
        artifactEntry.setRepositoryId(repositoryId);

        LocalDateTime now = LocalDateTimeInstance.now();

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);
        if(artifactEntry.getArtifactCoordinates()==null||!"debian".equals(repositoryPath.getRepository().getLayout())){
            artifactEntry.setArtifactCoordinates(coordinates);
        }
        artifactEntry.setCreated(now);
        artifactEntry.setLastUpdated(now);
        artifactEntry.setLastUsed(now);

        String username = UserUtils.getUsername();
        artifactEntry.setCreatedBy(username);
        artifactEntry.setUpdatedBy(username);
        artifactEntry.setArtifactName(FilenameUtils.getName(repositoryPath.toString()));
        artifactEntry.setArtifactPath(artifactEntry.getArtifactCoordinates().buildPath());
        repositoryPath.artifact = artifactEntry;
    }

    @Override
    public void onAfterWrite(RepositoryStreamWriteContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Complete writing [{}]", repositoryPath);

        if (RepositoryFiles.isArtifact(repositoryPath)) {
            if (ctx.getArtifactExists()) {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(repositoryPath);
            } else {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(repositoryPath);
            }
        } else if (RepositoryFiles.isMetadata(repositoryPath)) {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(repositoryPath);
        }
    }

    @Override
    public void onBeforeRead(RepositoryStreamReadContext ctx)
            throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Reading {}", repositoryPath);

        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            return;
        }

//        artifactEventListenerRegistry.dispatchArtifactDownloadingEvent(repositoryPath);
    }

    @Override
    public void onAfterRead(RepositoryStreamReadContext ctx) {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();

        logger.debug("Complete reading [{}]", repositoryPath);

//        artifactEventListenerRegistry.dispatchArtifactDownloadedEvent(repositoryPath);
    }

    @Override
    public void commit(RepositoryStreamWriteContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (artifact == null) {
            return;
        }
        String username = UserUtils.getUsername();
        LocalDateTime now = LocalDateTimeInstance.now();
        if (Objects.isNull(artifact.getNativeId())) {
            artifact.setCreated(now);
        }
        artifact.setLastUpdated(now);
        artifact.setLastUsed(now);
        artifact.setUpdatedBy(username);
        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);

        CountingOutputStream cos = StreamUtils.findSource(CountingOutputStream.class, ctx.getStream());
        artifact.setSizeInBytes(cos.getByteCount());

        LayoutOutputStream los = StreamUtils.findSource(LayoutOutputStream.class, ctx.getStream());
        artifact.setChecksums(los.getDigestMap(repository.getLayout()));

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactGroupWithTag(storage.getId(),
                repository.getId(),
                coordinates.getId(),
                Optional.of(lastVersionTag))
                .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                        repository.getId(),
                        coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, artifact);
        logger.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());
        try {
            artifactIdGroupRepository.saveOrUpdate(artifactGroup);
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            logger.warn("[{}] [{}] merge group error [{}]",
                    this.getClass().getSimpleName(), repositoryPath, realMessage);
            if (CommonUtils.catchException(realMessage)) {
                logger.warn("[{}] [{}] merge group catch error",
                        this.getClass().getSimpleName(), repositoryPath);
                return;
            }
            throw ex;
        }
    }

    @Override
    public void commitStoreIndex(RepositoryStreamReadContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (artifact == null) {
            artifact = provideArtifact(repositoryPath);
            if (artifact == null) {
                return;
            }
        }
        if (!shouldStoreArtifact(artifact)) {
            return;
        }
        String username = UserUtils.getUsername();
        //获取文件创建时间当做制品的创建时间
        LocalDateTime createTime = RepositoryPathUtil.getFileCreationTime(repositoryPath);
        if (Objects.isNull(createTime)) {
            createTime = LocalDateTime.now();
        }
        artifact.setCreated(createTime);
        artifact.setCreatedBy(username);
        artifact.setUpdatedBy(username);
        artifact.setLastUpdated(createTime);
        artifact.setLastUsed(createTime);
        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);
        artifact.setArtifactName(FilenameUtils.getName(repositoryPath.toString()));
        artifact.setArtifactPath(coordinates.buildPath());
        artifact.setSizeInBytes(Files.size(repositoryPath));

        LayoutInputStream lis = StreamUtils.findSource(LayoutInputStream.class, ctx.getStream());
        artifact.setChecksums(lis.getDigestMap());

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactGroupWithTag(storage.getId(),
                repository.getId(),
                coordinates.getId(),
                Optional.of(lastVersionTag))
                .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                        repository.getId(),
                        coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, artifact);
        logger.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());

        artifactIdGroupRepository.saveOrUpdate(artifactGroup);

        repositoryPath.artifact = artifact;
    }

    @Override
    public void onStoreIndexAfter(RepositoryStreamReadContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Complete build index [{}]", repositoryPath);

        if (RepositoryFiles.isArtifact(repositoryPath)) {
            if (ctx.getArtifactExists()) {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(repositoryPath);
            } else {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(repositoryPath);
            }
        } else if (RepositoryFiles.isMetadata(repositoryPath)) {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(repositoryPath);
        }
    }

    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException {
        return Optional.ofNullable(repositoryPath.getArtifactEntry())
                .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                        RepositoryFiles.readCoordinates(repositoryPath)));
    }

    protected boolean shouldStoreArtifact(Artifact artifactEntry) {
        return artifactEntry.getNativeId() == null;
    }

    @Override
    public RepositoryPath fetchPath(Path repositoryPath)
            throws IOException {
        return fetchPath((RepositoryPath) repositoryPath);
    }

    protected abstract RepositoryPath fetchPath(RepositoryPath repositoryPath) throws IOException;

    protected Predicate createPredicate(String storageId,
                                        String repositoryId,
                                        Predicate predicate) {
        Predicate result = Predicate.of(ExpOperator.EQ.of("storageId", storageId))
                .and(Predicate.of(ExpOperator.EQ.of("repositoryId", repositoryId)));
        if (predicate.isEmpty()) {
            return result;
        }

        return result.and(predicate);
    }

    protected Selector<ArtifactEntity> createSelector(String storageId,
                                                      String repositoryId,
                                                      Predicate p) {
        Selector<ArtifactEntity> selector = new Selector<>(ArtifactEntity.class);
        selector.where(createPredicate(storageId, repositoryId, p));

        return selector;
    }

    protected String getBaseUrl(Repository repository) {
        return String.format("%s/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}
