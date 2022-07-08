package com.veadan.folib.configuration;

import com.veadan.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;
import com.veadan.folib.client.RemoteRepositoryRetryArtifactDownloadConfiguration;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class RemoteRepositoriesConfiguration
{

    private RemoteRepositoryRetryArtifactDownloadConfiguration remoteRepositoryRetryArtifactDownloadConfiguration;

    private int checkIntervalSeconds;

    private int heartbeatThreadsNumber;

    @JsonCreator
    public RemoteRepositoriesConfiguration()
    {

    }

    @JsonCreator
    public RemoteRepositoriesConfiguration(@JsonProperty("delegate") final MutableRemoteRepositoriesConfiguration delegate)
    {
        this.remoteRepositoryRetryArtifactDownloadConfiguration = immuteRemoteRepositoryRetryArtifactDownloadConfiguration(
                delegate.getRetryArtifactDownloadConfiguration());
        this.checkIntervalSeconds = delegate.getCheckIntervalSeconds();
        this.heartbeatThreadsNumber = delegate.getHeartbeatThreadsNumber();
    }

    public RemoteRepositoryRetryArtifactDownloadConfiguration getRemoteRepositoryRetryArtifactDownloadConfiguration()
    {
        return remoteRepositoryRetryArtifactDownloadConfiguration;
    }

    private RemoteRepositoryRetryArtifactDownloadConfiguration immuteRemoteRepositoryRetryArtifactDownloadConfiguration(final MutableRemoteRepositoryRetryArtifactDownloadConfiguration source)
    {
        return source != null ? new RemoteRepositoryRetryArtifactDownloadConfiguration(source) : null;
    }

    public int getCheckIntervalSeconds()
    {
        return checkIntervalSeconds;
    }

    public int getHeartbeatThreadsNumber()
    {
        return heartbeatThreadsNumber;
    }
}
