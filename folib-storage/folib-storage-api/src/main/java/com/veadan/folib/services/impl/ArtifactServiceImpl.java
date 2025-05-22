package com.veadan.folib.services.impl;

import com.google.common.collect.Sets;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.*;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.io.LayoutOutputStream;
import com.veadan.folib.io.StreamUtils;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.util.UserUtils;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.output.CountingOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.janusgraph.core.JanusGraphTransaction;
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
        Optional<Artifact> exist = artifactRepository.findById(artifact.getUuid());
        ArtifactEntity artifactEntity = (ArtifactEntity)artifact ;
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
