package com.veadan.folib.dto.cron;

import com.veadan.folib.validation.cron.CronTaskConfigurationDtoValid;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;


/**
 * @author veadan
 */
@CronTaskConfigurationDtoValid(message = "Invalid cron task configuration")
public class CronTaskConfigurationDto
{

    private String jobClass;                //用户选择哪个类

    private String cronExpression;         //表达式

    private boolean oneTimeExecution;      //false 循环执行，true执行1次

    private boolean immediateExecution;   //是否立即执行

    private List<CronTaskConfigurationDtoField> fields;

    public String getJobClass()
    {
        return jobClass;
    }

    public void setJobClass(String jobClass)
    {
        this.jobClass = jobClass;
    }

    public List<CronTaskConfigurationDtoField> getFields()
    {
        return ObjectUtils.defaultIfNull(fields, Collections.emptyList());
    }

    public void setFields(List<CronTaskConfigurationDtoField> fields)
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
