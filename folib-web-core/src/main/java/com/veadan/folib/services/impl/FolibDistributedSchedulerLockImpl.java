package com.veadan.folib.services.impl;

import com.veadan.folib.cluster.FolibLockProperties;
import com.veadan.folib.entity.FolibLock;
import com.veadan.folib.mapper.FolibLockMapper;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.FolibDistributedSchedulerLock;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

@Service
public class FolibDistributedSchedulerLockImpl implements FolibDistributedSchedulerLock {
    private static final Logger logger = LoggerFactory.getLogger(
            FolibDistributedSchedulerLockImpl.class);
    @Autowired
    private FolibLockMapper folibLockMapper;

    @Autowired
    private FolibLockProperties properties;

    private ClusterSyncService clusterSyncService;

    @Override
    public Boolean getLock(String name, Long lockAtMostSeconds) {
        boolean result = false;
        try {
            if (!clusterSyncService.clusterOpenFlag()) {
                return true;
            }
            FolibLock folibLock = folibLockMapper.selectFolibLock(name);
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss", Locale.ROOT);
            LocalDateTime localDateTime = LocalDateTimeInstance.now();
            String lockUntil = localDateTime.plusSeconds(lockAtMostSeconds).format(formatter);
            String lockedAt = localDateTime.format(formatter);
            if (null == folibLock) {
                folibLockMapper.insertLock(name, lockUntil, lockedAt, properties.getFolibLockIp());
                // 查询ip 是否匹配
                FolibLock reFolibLock = folibLockMapper.selectFolibLock(name);
                return null != reFolibLock && reFolibLock.getLockedBy().equals(properties.getFolibLockIp());
            } else {
                // 查询当前local 主机
                if (folibLock.getLockedBy().equals(properties.getFolibLockIp())) {
                    //延长锁时间
                    folibLockMapper.updateFolibLock(name, lockUntil, lockedAt, properties.getFolibLockIp());
                    logger.info("task [{}] get lock by {} lockAt {} lockUntil {}", name, properties.getFolibLockIp(), lockedAt, lockUntil);
                    return true;
                } else {
                    // 比对占用锁的时间是否失效
                    if (folibLock.getLockUntil().toLocalDateTime().isBefore(localDateTime)) {
                        //重新抢锁
                        releaseLock(name);
                        Thread.sleep(10000);
                        getLock(name, lockAtMostSeconds);
                    }
                }

            }
        } catch (Throwable e) {
            logger.error(e.getMessage());
        }
        return result;
    }

    @Override
    public int releaseLock(String name) {
        return folibLockMapper.deleteFolibLock(name);
    }
}
