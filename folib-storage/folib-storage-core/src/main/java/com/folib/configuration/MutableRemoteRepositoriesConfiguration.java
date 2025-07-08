package com.folib.configuration;

import com.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;

import java.io.Serializable;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableRemoteRepositoriesConfiguration
        implements Serializable
{

    public static final int DEFAULT_HEARTBEAT_INTERVAL_SECONDS = 60;

    public static final MutableRemoteRepositoriesConfiguration DEFAULT = new MutableRemoteRepositoriesConfiguration()
    {

        @Override
        public void setCheckIntervalSeconds(int checkIntervalSeconds)
        {
            throw new UnsupportedOperationException("DEFAULT RemoteRepositoriesConfiguration is immutable");
        }

        @Override
        public void setHeartbeatThreadsNumber(int heartbeatThreadsNumber)
        {
            throw new UnsupportedOperationException("DEFAULT RemoteRepositoriesConfiguration is immutable");
        }
    };

    private MutableRemoteRepositoryRetryArtifactDownloadConfiguration retryArtifactDownloadConfiguration = MutableRemoteRepositoryRetryArtifactDownloadConfiguration.DEFAULT;

    private int checkIntervalSeconds = DEFAULT_HEARTBEAT_INTERVAL_SECONDS;

    private int heartbeatThreadsNumber = 5;

    public MutableRemoteRepositoryRetryArtifactDownloadConfiguration getRetryArtifactDownloadConfiguration()
    {
        return retryArtifactDownloadConfiguration;
    }

    public void setRetryArtifactDownloadConfiguration(final MutableRemoteRepositoryRetryArtifactDownloadConfiguration retryArtifactDownloadConfiguration)
    {
        this.retryArtifactDownloadConfiguration = retryArtifactDownloadConfiguration;
    }

    public int getCheckIntervalSeconds()
    {
        return checkIntervalSeconds;
    }

    public void setCheckIntervalSeconds(int checkIntervalSeconds)
    {
        this.checkIntervalSeconds = checkIntervalSeconds;
    }

    public int getHeartbeatThreadsNumber()
    {
        return heartbeatThreadsNumber;
    }

    public void setHeartbeatThreadsNumber(int heartbeatThreadsNumber)
    {
        this.heartbeatThreadsNumber = heartbeatThreadsNumber;
    }
}
