package com.veadan.folib.services.impl;

import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.util.CommonUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.util.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
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
    DistributedLockComponent distributedLockComponent;

    @Override
    public void saveOrUpdateArtifact(Artifact artifact) {
        if (distributedLockComponent.lock(artifact.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    Thread.sleep(100L);
                } catch (Exception ex) {

                }
                Graph g = janusGraph.tx().createThreadedTx();
                try {
                    artifact.setLastUpdated(LocalDateTimeInstance.now());
                    artifact.setLastUsed(artifact.getLastUpdated());
                    artifact.setUpdatedBy(UserUtils.getUsername());
                    artifactRepository.merge(() -> g.traversal(EntityTraversalSource.class), artifact);
                    if (g.tx().isOpen()) {
                        g.tx().commit();
                    }
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
    public Artifact findArtifactReport(RepositoryPath repositoryPath) throws IOException {
        return artifactRepository.findArtifactReport(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
    }
}
