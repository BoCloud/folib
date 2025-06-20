package com.veadan.folib.job.cron.exceptions;

/**
 * @author Veadan
 */
public class CronTaskUUIDNotUniqueException
        extends RuntimeException
{

    public CronTaskUUIDNotUniqueException(final String message)
    {
        super(message);
    }
}
