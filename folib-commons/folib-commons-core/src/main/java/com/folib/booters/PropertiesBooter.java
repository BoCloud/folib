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
package com.folib.booters;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class PropertiesBooter
{
    @Value("${folib.home}")
    private String homeDirectory;

    @Value("${folib.vault}")
    private String vaultDirectory;

    @Value("${folib.etc}")
    private String etcDirectory;

    @Value("${logging.dir}")
    private String logsDirectory;

    @Value("${folib.storage.booter.basedir}")
    private String storageBooterBasedir;

    @Value("${folib.config.file}")
    private String configFile;

    @Value("${folib.host:localhost}")
    private String host;

    @Value("${folib.port}")
    private int port;

    @Value("${folib.nuget.download.feed}")
    private boolean folibNugetDownloadFeed;

    @Value("${folib.version}")
    private String folibVersion;

    @Value("${folib.revision}")
    private String folibRevision;

    public String getHomeDirectory()
    {
        return homeDirectory;
    }

    public void setHomeDirectory(String homeDirectory)
    {
        this.homeDirectory = homeDirectory;
    }

    public String getVaultDirectory()
    {
        return vaultDirectory;
    }

    public void setVaultDirectory(String vaultDirectory)
    {
        this.vaultDirectory = vaultDirectory;
    }

    public String getEtcDirectory()
    {
        return etcDirectory;
    }

    public void setEtcDirectory(String etcDirectory)
    {
        this.etcDirectory = etcDirectory;
    }

    public String getLogsDirectory()
    {
        return logsDirectory;
    }

    public void setLogsDirectory(String logsDirectory)
    {
        this.logsDirectory = logsDirectory;
    }

    public String getStorageBooterBasedir()
    {
        return storageBooterBasedir;
    }

    public void setStorageBooterBasedir(String storageBooterBasedir)
    {
        this.storageBooterBasedir = storageBooterBasedir;
    }

    public String getConfigFile()
    {
        return configFile;
    }

    public void setConfigFile(String configFile)
    {
        this.configFile = configFile;
    }

    public String getHost()
    {
        return host;
    }

    public void setHost(String host)
    {
        this.host = host;
    }

    public int getPort()
    {
        return port;
    }

    public void setPort(int port)
    {
        this.port = port;
    }

    public boolean shouldDownloadFolibNugetFeed()
    {
        return folibNugetDownloadFeed;
    }

    public void setFolibNugetDownloadFeed(boolean folibNugetDownloadFeed)
    {
        this.folibNugetDownloadFeed = folibNugetDownloadFeed;
    }

    public String getFolibVersion()
    {
        return folibVersion;
    }

    public void setFolibVersion(String folibVersion)
    {
        this.folibVersion = folibVersion;
    }

    public String getFolibRevision()
    {
        return folibRevision;
    }

    public void setFolibRevision(String folibRevision)
    {
        this.folibRevision = folibRevision;
    }
}
