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
