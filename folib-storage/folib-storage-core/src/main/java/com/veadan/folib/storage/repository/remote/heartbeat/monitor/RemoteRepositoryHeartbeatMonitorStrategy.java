package com.veadan.folib.storage.repository.remote.heartbeat.monitor;

/**
 * @author Przemyslaw Fusik
 */
@FunctionalInterface
public interface RemoteRepositoryHeartbeatMonitorStrategy
{

    boolean isAlive(String remoteRepositoryUrl);
}
