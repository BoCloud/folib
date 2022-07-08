package com.veadan.folib.storage.repository.remote.heartbeat.monitor;

/**
 * @author veadan
 */
@FunctionalInterface
public interface RemoteRepositoryHeartbeatMonitorStrategy
{

    boolean isAlive(String remoteRepositoryUrl);
}
