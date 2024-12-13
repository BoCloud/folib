package com.veadan.folib.services.impl;

import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.util.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;
import java.util.Objects;
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

    @Override
    public void saveOrUpdateArtifact(Artifact artifact) {
        saveOrUpdateArtifact(artifact, true);
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
                    log.warn("写入制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.warn("StoreArtifactMetadataFile error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }
}
