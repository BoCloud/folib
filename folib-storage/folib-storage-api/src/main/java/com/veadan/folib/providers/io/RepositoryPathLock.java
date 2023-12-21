package com.veadan.folib.providers.io;

import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.constant.GlobalConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import java.io.IOException;
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
        logger.info("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, 30L, TimeUnit.SECONDS, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public void unLock(String lockName) {
        if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
            logger.info("Unlocked for [{}]", lockName);
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
        return String.format("%s-%s-%s", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
    }

    public int getLockInfo() {
        return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).size();
    }

}
