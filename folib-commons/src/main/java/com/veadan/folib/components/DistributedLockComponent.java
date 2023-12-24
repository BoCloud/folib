package com.veadan.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.constant.GlobalConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.concurrent.TimeUnit;

/**
 * @author leipenghui
 * @date 2023/12/12
 **/
@Slf4j
@Component
public class DistributedLockComponent {

    @Inject
    private HazelcastInstance hazelcastInstance;

    public boolean lock(String lockName) {
        log.debug("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, 30L, TimeUnit.SECONDS, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public boolean lock(String lockName, long waitTime, TimeUnit timeUnit) {
        log.debug("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, waitTime, timeUnit, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public boolean lock(String lockName, long waitTime) {
        return lock(lockName, waitTime, TimeUnit.SECONDS);
    }

    public void unLock(String lockName) {
        if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
            log.info("Unlocked for [{}]", lockName);
        } else {
            log.warn("LockName for [{}] Not locked", lockName);
        }
    }

    public void unLock(String lockName, long waitTime) {
        try {
            Thread.sleep(waitTime);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        } finally {
            if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
                hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
                log.info("Unlocked for [{}]", lockName);
            } else {
                log.warn("LockName for [{}] Not locked", lockName);
            }
        }
    }
}
