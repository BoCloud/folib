package com.folib.storage.repository.remote.heartbeat.monitor;

import static com.folib.utils.Ping.pingHost;

/**
 * @author veadan
 */
enum PingRemoteRepositoryUrlStrategy
        implements RemoteRepositoryHeartbeatMonitorStrategy
{

    INSTANCE;

    @Override
    public boolean isAlive(String storageId, String repositoryId, String remoteRepositoryUrl)
    {
        return pingHost(remoteRepositoryUrl, 5000);
    }
}
