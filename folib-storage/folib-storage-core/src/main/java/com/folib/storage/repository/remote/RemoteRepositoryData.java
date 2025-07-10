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

import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import javax.annotation.concurrent.Immutable;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class RemoteRepositoryData
        implements RemoteRepository
{

    private boolean downloadRemoteIndexes;

    private boolean autoBlocking;

    private boolean checksumValidation;

    private String username;

    private String password;

    private String checksumPolicy;

    private Integer checkIntervalSeconds;

    private boolean allowsDirectoryBrowsing;

    private boolean autoImportRemoteSSLCertificate;

    private String url;

    private CustomRemoteRepositoryConfigurationData customConfiguration;

    RemoteRepositoryData()
    {

    }

    public RemoteRepositoryData(final RemoteRepositoryDto other)
    {
        this.url = other.getUrl();
        this.downloadRemoteIndexes = other.isDownloadRemoteIndexes();
        this.autoBlocking = other.isAutoBlocking();
        this.checksumValidation = other.isChecksumValidation();
        this.username = other.getUsername();
        this.password = other.getPassword();
        this.checksumPolicy = other.getChecksumPolicy();
        this.checkIntervalSeconds = other.getCheckIntervalSeconds();
        this.allowsDirectoryBrowsing = other.allowsDirectoryBrowsing();
        this.autoImportRemoteSSLCertificate = other.isAutoImportRemoteSSLCertificate();
        this.customConfiguration = immuteRemoteRepositoryConfiguration(other.getCustomConfiguration());
    }

    public String getUrl()
    {
        return url;
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

    public boolean allowsDirectoryBrowsing()
    {
        return allowsDirectoryBrowsing;
    }

    public boolean isAutoImportRemoteSSLCertificate()
    {
        return autoImportRemoteSSLCertificate;
    }

    public CustomRemoteRepositoryConfigurationData getCustomConfiguration()
    {
        return customConfiguration;
    }

    private CustomRemoteRepositoryConfigurationData immuteRemoteRepositoryConfiguration(final RemoteRepositoryConfigurationDto source)
    {
        return source != null ? source.getImmutable() : null;
    }

}
