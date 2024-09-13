package com.veadan.folib.storage.repository.remote.heartbeat;

import java.util.Objects;

import javax.annotation.Nonnull;
import javax.validation.constraints.NotNull;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.monitor.RemoteRepositoryHeartbeatMonitorStrategy;
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
