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
package com.folib.forms.cron;

import com.folib.validation.cron.CronTaskConfigurationFormValid;
import org.apache.commons.lang3.ObjectUtils;

import java.util.Collections;
import java.util.List;


/**
 * @author veadan
 */
@CronTaskConfigurationFormValid(message = "Invalid cron task configuration")
public class CronTaskConfigurationForm
{

    private String jobClass;                //用户选择哪个类

    private String cronExpression;         //表达式

    private boolean oneTimeExecution;      //false 循环执行，true执行1次

    private boolean immediateExecution;   //是否立即执行

    private List<CronTaskConfigurationFormField> fields;

    public String getJobClass()
    {
        return jobClass;
    }

    public void setJobClass(String jobClass)
    {
        this.jobClass = jobClass;
    }

    public List<CronTaskConfigurationFormField> getFields()
    {
        return ObjectUtils.defaultIfNull(fields, Collections.emptyList());
    }

    public void setFields(List<CronTaskConfigurationFormField> fields)
    {
        this.fields = fields;
    }

    public String getCronExpression()
    {
        return cronExpression;
    }

    public void setCronExpression(String cronExpression)
    {
        this.cronExpression = cronExpression;
    }

    public boolean isOneTimeExecution()
    {
        return oneTimeExecution;
    }

    public void setOneTimeExecution(boolean oneTimeExecution)
    {
        this.oneTimeExecution = oneTimeExecution;
    }

    public boolean isImmediateExecution()
    {
        return immediateExecution;
    }

    public void setImmediateExecution(boolean immediateExecution)
    {
        this.immediateExecution = immediateExecution;
    }
}
