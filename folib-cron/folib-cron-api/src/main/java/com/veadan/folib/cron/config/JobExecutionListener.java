package com.veadan.folib.cron.config;

public interface JobExecutionListener
{

    void onJobExecution(String jobName,
                        Boolean statusExecuted);
}
