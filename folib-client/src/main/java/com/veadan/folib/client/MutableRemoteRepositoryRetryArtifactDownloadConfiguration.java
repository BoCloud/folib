package com.veadan.folib.client;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;

/**
 */
@XmlRootElement(name = "retry-artifact-download-configuration")
@XmlAccessorType(XmlAccessType.FIELD)
public class MutableRemoteRepositoryRetryArtifactDownloadConfiguration
        implements Serializable
{

    public static final MutableRemoteRepositoryRetryArtifactDownloadConfiguration DEFAULT = new MutableRemoteRepositoryRetryArtifactDownloadConfiguration()
    {
        @Override
        public void setMaxNumberOfAttempts(final int maxNumberOfAttempts)
        {
            throw new UnsupportedOperationException("DEFAULT RemoteRepositoryRetryArtifactDownloadConfiguration is immutable");
        }

        @Override
        public void setMinAttemptsIntervalSeconds(final int minAttemptsIntervalSeconds)
        {
            throw new UnsupportedOperationException("DEFAULT RemoteRepositoryRetryArtifactDownloadConfiguration is immutable");
        }

        @Override
        public void setTimeoutSeconds(final int timeoutSeconds)
        {
            throw new UnsupportedOperationException("DEFAULT RemoteRepositoryRetryArtifactDownloadConfiguration is immutable");
        }
    };

    @XmlAttribute(name = "timeout-seconds")
    private int timeoutSeconds = 60;

    @XmlAttribute(name = "max-number-of-attempts")
    private int maxNumberOfAttempts = 5;

    @XmlAttribute(name = "min-attempts-interval-seconds")
    private int minAttemptsIntervalSeconds = 5;

    public int getTimeoutSeconds()
    {
        return timeoutSeconds;
    }

    public void setTimeoutSeconds(final int timeoutSeconds)
    {
        this.timeoutSeconds = timeoutSeconds;
    }

    public int getMaxNumberOfAttempts()
    {
        return maxNumberOfAttempts;
    }

    public void setMaxNumberOfAttempts(final int maxNumberOfAttempts)
    {
        this.maxNumberOfAttempts = maxNumberOfAttempts;
    }

    public int getMinAttemptsIntervalSeconds()
    {
        return minAttemptsIntervalSeconds;
    }

    public void setMinAttemptsIntervalSeconds(final int minAttemptsIntervalSeconds)
    {
        this.minAttemptsIntervalSeconds = minAttemptsIntervalSeconds;
    }
}
