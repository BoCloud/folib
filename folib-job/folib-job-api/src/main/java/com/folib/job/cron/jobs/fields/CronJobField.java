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
package com.folib.job.cron.jobs.fields;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
@JsonSerialize(using = CronJobFieldJsonSerializer.class)
public abstract class CronJobField
{

    private final CronJobField field;

    public CronJobField(CronJobField field)
    {
        this.field = field;
    }

    public abstract String getKey();

    public abstract String getValue();

    protected CronJobField getField()
    {
        return field;
    }

    public boolean isRequired()
    {
        return field != null && field.isRequired();
    }

    public String getName()
    {
        Assert.notNull(field, () -> String.format("Field %s does not have name", field));
        return field.getName();
    }

    public String getAliasName()
    {
        return field.getAliasName();
    }

    public String getType()
    {
        Assert.notNull(field, () -> String.format("Field %s does not have type", field));
        return field.getType();
    }

    public String getAutocompleteValue()
    {
        if (field == null)
        {
            return null;
        }
        return field.getAutocompleteValue();
    }
}
