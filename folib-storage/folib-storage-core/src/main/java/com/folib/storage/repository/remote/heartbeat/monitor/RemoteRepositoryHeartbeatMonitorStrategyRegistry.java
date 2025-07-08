package com.folib.storage.repository.remote.heartbeat.monitor;

import jakarta.inject.Inject;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class RemoteRepositoryHeartbeatMonitorStrategyRegistry
{

    @Inject
    private RemoteRepositoryHeartbeatMonitorStrategy httpGetRemoteRepositoryCheckStrategy;


    public RemoteRepositoryHeartbeatMonitorStrategy of(boolean allowsDirectoryBrowsing)
    {
        return allowsDirectoryBrowsing ? httpGetRemoteRepositoryCheckStrategy :
               PingRemoteRepositoryUrlStrategy.INSTANCE;
    }

}
