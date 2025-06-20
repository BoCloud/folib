package com.veadan.folib.job.event.cron;

/**
 * @author Veadan
 */
public enum CronTaskEventTypeEnum
{

    /**
     * Occurs when the server has started initializing.
     */
    EVENT_CRON_TASK_SAVED(1),

    /**
     * Occurs when the server has begun a graceful shutdown.
     */
    EVENT_CRON_TASK_DELETED(2),

    /**
     * Occurs when the server has stopped.
     */
    EVENT_CRON_TASK_EXECUTING(3),

    /**
     * Occurs when the server's configuration has been changed.
     */
    EVENT_CRON_TASK_EXECUTION_COMPLETE(4);

    private int type;


    CronTaskEventTypeEnum(int type)
    {
        this.type = type;
    }

    public int getType()
    {
        return type;
    }

}
