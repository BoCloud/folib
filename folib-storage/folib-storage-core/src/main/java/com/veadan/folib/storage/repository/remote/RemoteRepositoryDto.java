package com.veadan.folib.storage.repository.remote;

import com.veadan.folib.configuration.MutableRemoteRepositoriesConfiguration;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author veadan
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RemoteRepositoryDto
        implements RemoteRepository
{

    private String url;

    private boolean downloadRemoteIndexes;

    private boolean autoBlocking;

    private boolean checksumValidation;

    private String username;

    private String password;

    private String checksumPolicy;

    private Integer checkIntervalSeconds = MutableRemoteRepositoriesConfiguration.DEFAULT_HEARTBEAT_INTERVAL_SECONDS;

    private boolean allowsDirectoryBrowsing = true;

    private boolean autoImportRemoteSSLCertificate;

    private RemoteRepositoryConfigurationDto customConfiguration;

    public String getUrl()
    {
        return url;
    }

    public void setUrl(String url)
    {
        this.url = url;
    }

    public boolean isDownloadRemoteIndexes()
    {
        return downloadRemoteIndexes;
    }

    public void setDownloadRemoteIndexes(boolean downloadRemoteIndexes)
    {
        this.downloadRemoteIndexes = downloadRemoteIndexes;
    }

    public boolean isAutoBlocking()
    {
        return autoBlocking;
    }

    public void setAutoBlocking(boolean autoBlocking)
    {
        this.autoBlocking = autoBlocking;
    }

    public boolean isChecksumValidation()
    {
        return checksumValidation;
    }

    public void setChecksumValidation(boolean checksumValidation)
    {
        this.checksumValidation = checksumValidation;
    }

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getChecksumPolicy()
    {
        return checksumPolicy;
    }

    public void setChecksumPolicy(String checksumPolicy)
    {
        this.checksumPolicy = checksumPolicy;
    }

    public Integer getCheckIntervalSeconds()
    {
        return checkIntervalSeconds;
    }

    public void setCheckIntervalSeconds(Integer checkIntervalSeconds)
    {
        this.checkIntervalSeconds = checkIntervalSeconds;
    }

    public void setAllowsDirectoryBrowsing(boolean allowsDirectoryBrowsing)
    {
        this.allowsDirectoryBrowsing = allowsDirectoryBrowsing;
    }

    public boolean isAutoImportRemoteSSLCertificate()
    {
        return autoImportRemoteSSLCertificate;
    }

    public void setAutoImportRemoteSSLCertificate(boolean autoImportRemoteSSLCertificate)
    {
        this.autoImportRemoteSSLCertificate = autoImportRemoteSSLCertificate;
    }

    public boolean allowsDirectoryBrowsing()
    {
        return allowsDirectoryBrowsing;
    }

    public RemoteRepositoryConfigurationDto getCustomConfiguration()
    {
        return customConfiguration;
    }

    public void setCustomConfiguration(RemoteRepositoryConfigurationDto customConfiguration)
    {
        this.customConfiguration = customConfiguration;
    }

}
