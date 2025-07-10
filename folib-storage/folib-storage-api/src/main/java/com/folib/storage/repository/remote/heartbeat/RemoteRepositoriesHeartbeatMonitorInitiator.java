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

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.*;
import java.util.stream.Collectors;

import jakarta.inject.Inject;

import com.folib.configuration.ConfigurationManager;
import org.apache.commons.lang3.ObjectUtils;
import com.folib.log.CronTaskContextAcceptFilter;
import com.folib.log.LoggingUtils;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.monitor.RemoteRepositoryHeartbeatMonitorStrategy;
import com.folib.storage.repository.remote.heartbeat.monitor.RemoteRepositoryHeartbeatMonitorStrategyRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
@Component
public class RemoteRepositoriesHeartbeatMonitorInitiator
        implements InitializingBean, DisposableBean
{

    private static final Logger logger = LoggerFactory.getLogger(RemoteRepositoriesHeartbeatMonitorInitiator.class);

    private ScheduledExecutorService executor;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryCacheManager;

    @Inject
    private RemoteRepositoryHeartbeatMonitorStrategyRegistry remoteRepositoryHeartbeatMonitorStrategyRegistry;

    private final Map<String, ScheduledFuture<?>> scheduledTasks = new ConcurrentHashMap<>();

    @Override
    public void destroy()
    {
        executor.shutdown();
    }

    @Override
    public void afterPropertiesSet()
    {
        int heartbeatThreadsNumber = getRemoteRepositoriesHeartbeatThreadsNumber();
        executor = Executors.newScheduledThreadPool(heartbeatThreadsNumber);

        int defaultIntervalSeconds = getDefaultRemoteRepositoriesHeartbeatIntervalSeconds();

        getRemoteRepositories().stream().forEach(rr -> scheduleRemoteRepositoryMonitoring(defaultIntervalSeconds, rr));
    }

    public void scheduleRemoteRepositoryMonitoring(int defaultIntervalSeconds,
                                                    String storageAndRepositoryId)
    {
        Repository repository = configurationManager.getRepository(storageAndRepositoryId);
        if (Objects.isNull(repository)) {
            return;
        }
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (Objects.isNull(remoteRepository)) {
            return;
        }
        int intervalSeconds = ObjectUtils.defaultIfNull(remoteRepository.getCheckIntervalSeconds(),
                                                        defaultIntervalSeconds);

        Assert.isTrue(intervalSeconds > 0,
                      "intervalSeconds cannot be negative or zero but was " + intervalSeconds + " for " +
                      remoteRepository.getUrl());

        RemoteRepositoryHeartbeatMonitor remoteRepositoryHeartBeatMonitor = new RemoteRepositoryHeartbeatMonitor(remoteRepositoryCacheManager,
                                                                                                                 determineMonitorStrategy(remoteRepository),
                                                                                                                 storageAndRepositoryId);
        ScheduledFuture<?> scheduledTask = executor.scheduleWithFixedDelay(new MdcContextProvider(remoteRepositoryHeartBeatMonitor),
                                        0,
                                        intervalSeconds, TimeUnit.SECONDS);
        scheduledTasks.put(storageAndRepositoryId, scheduledTask);
        logger.info("Remote repository {} scheduled for monitoring with interval seconds {}",
                    remoteRepository.getUrl(), intervalSeconds);
    }

    public void cancelRemoteRepositoryMonitoring(String storageAndRepositoryId) {
        ScheduledFuture<?> scheduledTask = scheduledTasks.remove(storageAndRepositoryId);
        if (scheduledTask != null) {
            // 取消定时任务
            scheduledTask.cancel(true);
            logger.info("Remote repository {} monitoring cancelled", storageAndRepositoryId);
        }
    }

    private RemoteRepositoryHeartbeatMonitorStrategy determineMonitorStrategy(final RemoteRepository remoteRepository)
    {
        return remoteRepositoryHeartbeatMonitorStrategyRegistry.of(remoteRepository.allowsDirectoryBrowsing());
    }


    private List<String> getRemoteRepositories()
    {
        return configurationManager.getConfiguration()
                                   .getStorages()
                                   .values()
                                   .stream()
                                   .flatMap(s -> s.getRepositories().values().stream())
                                   .filter(Repository::isProxyRepository)
                                   .map(r -> r.getStorageIdAndRepositoryId())
                                   .collect(Collectors.toList());
    }

    public int getDefaultRemoteRepositoriesHeartbeatIntervalSeconds()
    {
        return configurationManager.getConfiguration().getRemoteRepositoriesConfiguration().getCheckIntervalSeconds();
    }

    private int getRemoteRepositoriesHeartbeatThreadsNumber()
    {
        return configurationManager.getConfiguration().getRemoteRepositoriesConfiguration().getHeartbeatThreadsNumber();
    }
    
    public static class MdcContextProvider implements Runnable
    {

        private Runnable target;

        public MdcContextProvider(Runnable target)
        {
            super();
            this.target = target;
        }

        @Override
        public void run()
        {
            MDC.put(CronTaskContextAcceptFilter.FOLIB_CRON_CONTEXT_NAME, LoggingUtils.caclucateCronContextName(target.getClass()));
            try
            {
                target.run();
            } 
            finally
            {
                MDC.remove(CronTaskContextAcceptFilter.FOLIB_CRON_CONTEXT_NAME);
            }
        }

    }
}
