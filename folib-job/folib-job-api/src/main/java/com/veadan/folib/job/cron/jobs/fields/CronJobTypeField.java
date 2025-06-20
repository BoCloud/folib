package com.veadan.folib.job.cron.jobs.fields;

/**
 * @author veadan
 */
public abstract class CronJobTypeField
        extends CronJobField
{

    public CronJobTypeField()
    {
        this(null);
    }

    public CronJobTypeField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getKey()
    {
        return "type";
    }

    @Override
    public String getType()
    {
        return getValue();
    }
}
