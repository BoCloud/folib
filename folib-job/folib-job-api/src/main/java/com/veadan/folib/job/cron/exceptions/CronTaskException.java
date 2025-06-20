package com.veadan.folib.job.cron.exceptions;

/**
 * @author Yougeshwar
 */
public class CronTaskException
        extends Exception
{

    public CronTaskException()
    {
    }

    public CronTaskException(String msg)
    {
        super(msg);
    }

    public CronTaskException(String msg,
                             Throwable cause)
    {
        super(msg, cause);
    }

    public CronTaskException(Throwable cause)
    {
        super(cause);
    }

}
