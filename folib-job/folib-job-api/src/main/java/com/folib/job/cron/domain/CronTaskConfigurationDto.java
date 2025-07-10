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

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.util.Assert;

/**
 * @author Yougeshwar
 * @author Veadan
 * @author veadan
 */
public class CronTaskConfigurationDto
        implements Serializable
{

    private UUID uuid;

    private String name;

    private String jobClass;

    private String cronExpression;

    private boolean oneTimeExecution = false;

    private boolean immediateExecution = false;

    private Map<String, String> properties = new HashMap<>();

    public CronTaskConfigurationDto()
    {
    }

    @JsonCreator
    public CronTaskConfigurationDto(@JsonProperty(value = "uuid", required = true) UUID uuid,
                                    @JsonProperty(value = "name", required = true) String name)
    {
        this.uuid = uuid;
        this.name = name;
    }

    public UUID getUuid()
    {
        return uuid;
    }

    public void setUuid(UUID uuid)
    {
        this.uuid = uuid;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public void setProperties(Map<String, String> properties)
    {
        this.properties = properties;
    }

    public String getRequiredProperty(String key)
    {
        String value = getProperty(key);
        Assert.notNull(value, "No property of key '" + key + "' found");
        return value;
    }

    public String getProperty(String key)
    {
        return this.properties.get(key);
    }

    public void addProperty(String key,
                            String value)
    {
        properties.put(key, value);
    }

    public String getCronExpression()
    {
        return cronExpression;
    }

    public void setCronExpression(String cronExpression)
    {
        this.cronExpression = cronExpression;
    }

    public boolean contains(String key)
    {
        return properties.containsKey(key);
    }

    public boolean isOneTimeExecution()
    {
        return oneTimeExecution;
    }

    public void setOneTimeExecution(boolean oneTimeExecution)
    {
        this.oneTimeExecution = oneTimeExecution;
    }

    @JsonGetter("immediateExecution")
    public boolean shouldExecuteImmediately()
    {
        return immediateExecution;
    }

    public void setImmediateExecution(boolean immediateExecution)
    {
        this.immediateExecution = immediateExecution;
    }

    public String getJobClass()
    {
        return jobClass;
    }

    public void setJobClass(String jobClass)
    {
        this.jobClass = jobClass;
    }

    @Override
    public String toString()
    {
        return "CronTaskConfigurationDto{" +
               "uuid='" + uuid + '\'' +
               ", name='" + name + '\'' +
               ", jobClass='" + jobClass + '\'' +
               ", cronExpression='" + cronExpression + '\'' +
               ", oneTimeExecution=" + oneTimeExecution +
               ", immediateExecution=" + immediateExecution +
               ", properties=" + properties +
               '}';
    }

    @Override
    public boolean equals(final Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof CronTaskConfigurationDto))
        {
            return false;
        }

        final CronTaskConfigurationDto that = (CronTaskConfigurationDto) o;

        return getUuid() != null ? getUuid().equals(that.getUuid()) : that.getUuid() == null;
    }

    @Override
    public int hashCode()
    {
        return getUuid() != null ? getUuid().hashCode() : 0;
    }

    public boolean isImmediateExecution() {
        return immediateExecution;
    }
}
