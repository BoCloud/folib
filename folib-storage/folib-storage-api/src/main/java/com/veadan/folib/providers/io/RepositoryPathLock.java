package com.veadan.folib.providers.io;

import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.constant.GlobalConstants;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import java.io.IOException;
import java.net.URI;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 */
@Component
public class RepositoryPathLock {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryPathLock.class);

    @Inject
    private HazelcastInstance hazelcastInstance;

    public boolean lock(final @Nonnull RepositoryPath repositoryPath) throws IOException {
        return lock(repositoryPath, null);
    }

    public boolean lock(final @Nonnull RepositoryPath repositoryPath,
                        String id) throws IOException {
        URI lock = getLock(repositoryPath);
        String lockName = Optional.ofNullable(id)
                .map(p -> String.format("%s?%s", lock, p))
                .orElseGet(lock::toString);
        logger.info("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, 30L, TimeUnit.SECONDS, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    private URI getLock(final @Nonnull RepositoryPath repositoryPath) throws IOException {
        if (RepositoryFiles.isArtifact(repositoryPath)) {
            ArtifactCoordinates c = RepositoryFiles.readCoordinates(repositoryPath);
            // We should lock all the RepositoryArtifactIdGroup because there can be
            // `ArtifactEntryServiceImpl.updateLastVersionTag()` operations
            // which affetcs on other artifacts from group.
            return URI.create(UriUtils.encode(c.getId()));
        }

        final URI lock = repositoryPath.toUri();

        Assert.isTrue(lock.isAbsolute(), String.format("Unable to lock relative path %s", lock));

        return lock;
    }

    public void unLock(String lockName) {
        if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
            logger.info("Unlocked for [{}]", lockName);
        }
    }

    public void unLock(RepositoryPath repositoryPath) {
        try {
            URI lock = getLock(repositoryPath);
            String lockName = Optional.empty()
                    .map(p -> String.format("%s?%s", lock, p))
                    .orElseGet(lock::toString);
            unLock(lockName);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    public int getLockInfo() {
        return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).size();
    }

}
