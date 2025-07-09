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
package com.folib.forms.configuration;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;

/**
 * @author veadan
 */
public class RemoteRepositoryForm
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
