package com.folib.job.cron.config;

public interface JobExecutionListener
{

    void onJobExecution(String jobName,
                        Boolean statusExecuted);
}
