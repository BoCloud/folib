package com.veadan.folib.dto.configuration;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;

/**
 * @author veadan
 */
public class RemoteRepositoryDto
{

    @NotEmpty(message = "An url must be specified.")
    private String url;

    private boolean downloadRemoteIndexes;

    private boolean autoBlocking;

    private boolean checksumValidation;

    private String username;

    private String password;

    private String checksumPolicy;

    @NotNull(message = "A checkIntervalSeconds must be specified.")
    @PositiveOrZero(message = "The checkIntervalSeconds must be greater, or equal to zero.")
    private Integer checkIntervalSeconds;

    private boolean allowsDirectoryBrowsing = true;

    private boolean autoImportRemoteSSLCertificate;

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

    public boolean isAutoBlocking()
    {
        return autoBlocking;
    }

    public boolean isChecksumValidation()
    {
        return checksumValidation;
    }

    public String getUsername()
    {
        return username;
    }

    public String getPassword()
    {
        return password;
    }

    public String getChecksumPolicy()
    {
        return checksumPolicy;
    }

    public Integer getCheckIntervalSeconds()
    {
        return checkIntervalSeconds;
    }

    public void setCheckIntervalSeconds(Integer checkIntervalSeconds)
    {
        this.checkIntervalSeconds = checkIntervalSeconds;
    }

    public boolean isAllowsDirectoryBrowsing()
    {
        return allowsDirectoryBrowsing;
    }

    public boolean isAutoImportRemoteSSLCertificate()
    {
        return autoImportRemoteSSLCertificate;
    }

}
