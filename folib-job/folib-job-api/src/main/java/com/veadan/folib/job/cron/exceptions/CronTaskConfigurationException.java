package com.veadan.folib.job.cron.exceptions;

/**
 * @author veadan
 */
public class CronTaskConfigurationException
        extends RuntimeException
{

    public CronTaskConfigurationException(final String message)
    {
        super(message);
    }
}
