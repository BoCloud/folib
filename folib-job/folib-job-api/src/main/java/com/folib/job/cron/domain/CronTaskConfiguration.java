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
package com.folib.job.cron.domain;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.google.common.collect.ImmutableMap;

/**
 * @author veadan
 * @author Veadan
 */
@Immutable
public class CronTaskConfiguration
{

    private final UUID uuid;

    private final String name;

    private final String jobClass;

    private final String cronExpression;

    private final boolean oneTimeExecution;

    private final boolean immediateExecution;

    private final Map<String, String> properties;

    public CronTaskConfiguration(final CronTaskConfigurationDto source)
    {
        this.uuid = source.getUuid();
        this.name = source.getName();
        this.jobClass = source.getJobClass();
        this.cronExpression = source.getCronExpression();
        this.oneTimeExecution = source.isOneTimeExecution();
        this.immediateExecution = source.shouldExecuteImmediately();
        this.properties = immuteProperties(source.getProperties());
    }

    private Map<String, String> immuteProperties(final Map<String, String> source)
    {
        return source != null ? ImmutableMap.copyOf(source) : Collections.emptyMap();
    }

    public UUID getUuid()
    {
        return uuid;
    }

    public String getName()
    {
        return name;
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public boolean isOneTimeExecution()
    {
        return oneTimeExecution;
    }

    public String getProperty(String key)
    {
        return this.properties.get(key);
    }

    public boolean contains(String key)
    {
        return properties.containsKey(key);
    }

    public boolean shouldExecuteImmediately()
    {
        return immediateExecution;
    }

    public String getJobClass()
    {
        return jobClass;
    }

    public String getCronExpression()
    {
        return cronExpression;
    }

    public boolean isImmediateExecution()
    {
        return immediateExecution;
    }

    @Override
    public boolean equals(final Object o)
    {
        if (this == o) return true;
        if (!(o instanceof CronTaskConfiguration))
        {
            return false;
        }
        final CronTaskConfiguration that = (CronTaskConfiguration) o;
        return Objects.equals(uuid, that.uuid);
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(uuid);
    }
}
