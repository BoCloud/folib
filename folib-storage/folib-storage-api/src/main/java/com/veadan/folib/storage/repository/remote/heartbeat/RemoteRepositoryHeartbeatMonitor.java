package com.veadan.folib.storage.repository.remote.heartbeat;

import java.util.Objects;

import javax.annotation.Nonnull;

import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.monitor.RemoteRepositoryHeartbeatMonitorStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Przemyslaw Fusik
 */
class RemoteRepositoryHeartbeatMonitor
        implements Runnable
{

    private static final Logger logger = LoggerFactory.getLogger(RemoteRepositoryHeartbeatMonitor.class);

    private final RemoteRepository remoteRepository;

    private final RemoteRepositoryAlivenessService remoteRepositoryCacheManager;

    private final RemoteRepositoryHeartbeatMonitorStrategy monitorStrategy;

    RemoteRepositoryHeartbeatMonitor(@Nonnull RemoteRepositoryAlivenessService remoteRepositoryCacheManager,
                                     @Nonnull RemoteRepositoryHeartbeatMonitorStrategy monitorStrategy,
                                     @Nonnull RemoteRepository remoteRepository)
    {
        Objects.requireNonNull(remoteRepositoryCacheManager);
        Objects.requireNonNull(monitorStrategy);
        Objects.requireNonNull(remoteRepository);

        this.remoteRepositoryCacheManager = remoteRepositoryCacheManager;
        this.monitorStrategy = monitorStrategy;
        this.remoteRepository = remoteRepository;
    }

    @Override
    public void run()
    {
        boolean isAlive = false;
        try
        {
            isAlive = monitorStrategy.isAlive(remoteRepository.getUrl());
        }
        catch (Exception ex)
        {
            logger.error("Problem determining remote repository [{}] aliveness", remoteRepository.getUrl(), ex);
        }

        logger.debug("Thread name is [{}]. Remote repository [{}] is alive ? [{}]", Thread.currentThread().getName(),
                     remoteRepository.getUrl(),
                     isAlive);
        remoteRepositoryCacheManager.put(remoteRepository, isAlive);
    }
}
