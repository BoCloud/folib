package com.folib.providers.io;

import com.hazelcast.core.HazelcastInstance;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ProductTypeEnum;
import com.folib.util.UriUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import javax.annotation.Nonnull;
import jakarta.inject.Inject;

import java.io.IOException;
import java.net.URI;
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
        String lockName = id;
        if (StringUtils.isBlank(id)) {
            lockName = getLockName(repositoryPath);
        }
        logger.debug("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, 1200L, TimeUnit.SECONDS, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public void unLock(String lockName) {
        if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
            logger.debug("Unlocked for [{}]", lockName);
        } else {
            logger.debug("LockName for [{}] not locked", lockName);
        }
    }

    public void unLock(RepositoryPath repositoryPath) {
        try {
            final String lockName = getLockName(repositoryPath);
            unLock(lockName);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    private String getLockName(RepositoryPath repositoryPath) throws IOException {
        URI uri = getURI(repositoryPath);
        String lockName = uri.toString();
        if (ProductTypeEnum.Docker.getFoLibraryName().equals(repositoryPath.getRepository().getLayout())) {
            //docker布局
            final String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST.stream().anyMatch(artifactPath::startsWith)) {
                lockName = artifactPath;
            }
        }
        return String.format("%s:%s:%s", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), lockName);
    }

    public int getLockInfo() {
        return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).size();
    }

    private URI getURI(final @Nonnull RepositoryPath repositoryPath) throws IOException {
        if (RepositoryFiles.isArtifact(repositoryPath)) {
            ArtifactCoordinates c = RepositoryFiles.readCoordinates(repositoryPath);
            return URI.create(UriUtils.encode(c.getId()));
        }
        final URI uri = repositoryPath.toUri();
        Assert.isTrue(uri.isAbsolute(), String.format("Unable to lock relative path %s", uri));
        return uri;
    }

}
