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
