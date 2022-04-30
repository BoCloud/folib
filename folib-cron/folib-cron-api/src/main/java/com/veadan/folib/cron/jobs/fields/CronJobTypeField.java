package com.veadan.folib.cron.jobs.fields;

/**
 * @author Przemyslaw Fusik
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
