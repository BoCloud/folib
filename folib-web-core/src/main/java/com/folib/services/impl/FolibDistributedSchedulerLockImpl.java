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
package com.folib.services.impl;

import com.folib.cluster.FolibLockProperties;
import com.folib.entity.FolibLock;
import com.folib.mapper.FolibLockMapper;
import com.folib.services.FolibDistributedSchedulerLock;
import com.folib.util.LocalDateTimeInstance;
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



    @Override
    public Boolean getLock(String name, Long lockAtMostSeconds) {
        boolean result = false;
        try {
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
