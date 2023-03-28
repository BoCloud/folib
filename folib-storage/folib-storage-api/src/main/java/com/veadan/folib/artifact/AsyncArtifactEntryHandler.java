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
import lombok.extern.slf4j.Slf4j;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

@Slf4j
public abstract class AsyncArtifactEntryHandler {

    private static final Logger logger = LoggerFactory.getLogger(AsyncArtifactEntryHandler.class);

    @Inject
    private ArtifactRepository artifactEntityRepository;
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
        ReadWriteLock lockSource = repositoryPathLock.lock(repositoryPath);
        Lock lock = lockSource.writeLock();
        lock.lock();
        try {
            handleTransactional(repositoryPath);
        } finally {
            lock.unlock();
        }
    }

    private void handleTransactional(RepositoryPath repositoryPath) {
        Graph g = janusGraph.tx().createThreadedTx();
        try {
            Artifact result = handleEvent(repositoryPath);
            if (result == null) {
                logger.debug("No [{}] result for event [{}] and path [{}].",
                        Artifact.class.getSimpleName(),
                        AsyncArtifactEntryHandler.this.getClass().getSimpleName(),
                        repositoryPath);
                g.tx().rollback();
                return;
            }
            artifactEntityRepository.merge(() -> g.traversal(EntityTraversalSource.class), result);
            g.tx().commit();
        } catch (Throwable e) {
            g.tx().rollback();
            throw new UndeclaredThrowableException(e);
        } finally {
            g.tx().close();
        }
    }

    protected abstract Artifact handleEvent(RepositoryPath repositoryPath)
            throws IOException;

}
