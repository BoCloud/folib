package com.veadan.folib.job.cron.quartz;

import org.quartz.JobDetail;
import org.quartz.Trigger;

/**
 * @author Yougeshwar
 */
public class CronTask
{

    private JobDetail jobDetail;

    private Trigger trigger;

    private String scriptName;


    public JobDetail getJobDetail()
    {
        return jobDetail;
    }

    public void setJobDetail(JobDetail jobDetail)
    {
        this.jobDetail = jobDetail;
    }

    public Trigger getTrigger()
    {
        return trigger;
    }

    public void setTrigger(Trigger trigger)
    {
        this.trigger = trigger;
    }

    public String getScriptName()
    {
        return scriptName;
    }

    public void setScriptName(String scriptName)
    {
        this.scriptName = scriptName;
    }

}
