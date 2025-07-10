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
package com.folib.job.cron.jobs;

import com.folib.job.cron.jobs.fields.CronJobField;

import java.util.Set;

import org.springframework.util.Assert;

/**
 * @author veadan
 */
public class CronJobDefinition
{

    private String jobClass;

    private String name;

    private String scope;

    private String description;

    private Set<CronJobField> fields;

    public String getScope() {
        return scope;
    }

    public String getDescription()
    {
        return description;
    }
    public String getJobClass()
    {
        return jobClass;
    }

    public String getName()
    {
        return name;
    }

    public Set<CronJobField> getFields()
    {
        return fields;
    }



    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        CronJobDefinition that = (CronJobDefinition) o;

        return jobClass.equals(that.jobClass);
    }

    @Override
    public int hashCode()
    {
        return jobClass.hashCode();
    }

    private CronJobDefinition(Builder builder)
    {
        Assert.notNull(builder.jobClass, "jobClass should not be null");
        jobClass = builder.jobClass;
        name = builder.name;
        description=builder.description;
        fields = builder.fields;
        scope = builder.scope;

    }

    public static Builder newBuilder()
    {
        return new Builder();
    }


    public static final class Builder
    {

        private String jobClass;
        private String name;
        private String scope;
        private String description;
        private Set<CronJobField> fields;

        private Builder()
        {
        }

        public Builder jobClass(String val)
        {
            jobClass = val;
            return this;
        }

        public Builder name(String val)
        {
            name = val;
            return this;
        }
        public Builder scope(String val)
        {
            scope = val;
            return this;
        }

        public Builder description(String val)
        {
            description = val;
            return this;
        }

        public Builder fields(Set<CronJobField> val)
        {
            fields = val;
            return this;
        }

        public CronJobDefinition build()
        {
            return new CronJobDefinition(this);
        }
    }
}
