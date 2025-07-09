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
