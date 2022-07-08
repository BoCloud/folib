package com.veadan.folib.forms.cron;

import com.veadan.folib.validation.cron.CronTaskConfigurationFormValid;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;


/**
 * @author veadan
 */
@CronTaskConfigurationFormValid(message = "Invalid cron task configuration")
public class CronTaskConfigurationForm
{

    private String jobClass;

    private String cronExpression;

    private boolean oneTimeExecution;

    private boolean immediateExecution;

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
