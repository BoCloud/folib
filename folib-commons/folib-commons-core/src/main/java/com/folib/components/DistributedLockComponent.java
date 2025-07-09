/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.folib.constant.GlobalConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
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
            log.warn("LockName for [{}] not locked", lockName);
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
                log.warn("LockName for [{}] not locked", lockName);
            }
        }
    }

    public boolean lock(String lockName, long waitTime, TimeUnit timeUnit, long releaseTime, TimeUnit releaseTimeUnit) {
        log.debug("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, waitTime, timeUnit, releaseTime, releaseTimeUnit);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }
}
