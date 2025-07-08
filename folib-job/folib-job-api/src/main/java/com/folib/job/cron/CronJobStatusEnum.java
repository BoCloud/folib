package com.folib.job.cron;

/**
 * @author Veadan
 */
public enum CronJobStatusEnum
{

    /**
     * Use this status when the cron job is sleeping while waiting for the next time it should execute.
     */
    SLEEPING("Sleeping"),

    /**
     * Use this status when the cron job is executing it's task.
     */
    EXECUTING("Executing"),

    /**
     * Use this status when the cron job is blocked by another task.
     */
    BLOCKED("Blocked"),

    /**
     * Use this status when the cron job is disabled.
     */
    DISABLED("Disabled");

    String status;


    CronJobStatusEnum(String status)
    {
        this.status = status;
    }

    public String getStatus()
    {
        return status;
    }

}
