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
package com.folib.storage.repository.remote.heartbeat;

import java.util.Objects;

import javax.annotation.Nonnull;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.monitor.RemoteRepositoryHeartbeatMonitorStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
class RemoteRepositoryHeartbeatMonitor
        implements Runnable
{

    private static final Logger logger = LoggerFactory.getLogger(RemoteRepositoryHeartbeatMonitor.class);

    private final String storageAndRepositoryId;

    private final RemoteRepositoryAlivenessService remoteRepositoryCacheManager;

    private final RemoteRepositoryHeartbeatMonitorStrategy monitorStrategy;

    RemoteRepositoryHeartbeatMonitor(@Nonnull RemoteRepositoryAlivenessService remoteRepositoryCacheManager,
                                     @Nonnull RemoteRepositoryHeartbeatMonitorStrategy monitorStrategy,
                                     @Nonnull String storageAndRepositoryId)
    {
        Objects.requireNonNull(remoteRepositoryCacheManager);
        Objects.requireNonNull(monitorStrategy);
        Objects.requireNonNull(storageAndRepositoryId);

        this.remoteRepositoryCacheManager = remoteRepositoryCacheManager;
        this.monitorStrategy = monitorStrategy;
        this.storageAndRepositoryId = storageAndRepositoryId;
    }

    @Override
    public void run()
    {
        boolean isAlive = true;
        Repository repository = null;
        RemoteRepository remoteRepository = null;
        try
        {
            ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
            repository = configurationManager.getRepository(storageAndRepositoryId);
            if (Objects.isNull(repository) || !repository.isProxyRepository()) {
                return;
            }
            remoteRepository = repository.getRemoteRepository();
            if (Objects.isNull(remoteRepository)) {
                return;
            }
            isAlive = monitorStrategy.isAlive(repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
        }
        catch (Exception ex)
        {
            logger.error("Problem determining remote repository [{}] [{}] aliveness", storageAndRepositoryId, remoteRepository.getUrl(), ex);
        }

        logger.info("Thread name is [{}]. Remote repository [{}] [{}] is alive ? [{}]", Thread.currentThread().getName(),
                     storageAndRepositoryId,
                     remoteRepository.getUrl(),
                     isAlive);
        remoteRepositoryCacheManager.put(remoteRepository, isAlive);
    }
}
