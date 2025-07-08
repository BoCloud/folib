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
