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
package com.folib.client;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

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
