package com.veadan.folib.storage.repository.remote.heartbeat.monitor;

import static com.veadan.folib.utils.Ping.pingHost;

/**
 * @author veadan
 */
enum PingRemoteRepositoryUrlStrategy
        implements RemoteRepositoryHeartbeatMonitorStrategy
{

    INSTANCE;

    @Override
    public boolean isAlive(String remoteRepositoryUrl)
    {
        return pingHost(remoteRepositoryUrl, 5000);
    }
}
