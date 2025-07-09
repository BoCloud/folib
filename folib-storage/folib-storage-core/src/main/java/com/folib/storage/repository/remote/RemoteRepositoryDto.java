/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.repository.remote;

import com.folib.configuration.MutableRemoteRepositoriesConfiguration;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

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
