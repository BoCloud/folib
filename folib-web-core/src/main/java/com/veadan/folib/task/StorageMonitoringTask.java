package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.services.StorageMonitoringService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author veadan
 * 存储监控task
 */
@Slf4j
@Component
@EnableScheduling
public class StorageMonitoringTask {

    @Inject
    private StorageMonitoringService storageMonitoringService;

    @Autowired
    private DistributedLockComponent distributedLockComponent;

    /**
     * 每天1点
     */
    @Scheduled(cron = "0 0 1 * * ? ")
    public void run() {
        String lockName = "StorageMonitoring";
        long waitTime = 1L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                storageMonitoringService.updateStorageMonitoringData();
                log.info("StorageMonitoring thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 1500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }
}
