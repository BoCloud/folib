package com.veadan.folib.artifact;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.concurrent.TimeUnit;

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
