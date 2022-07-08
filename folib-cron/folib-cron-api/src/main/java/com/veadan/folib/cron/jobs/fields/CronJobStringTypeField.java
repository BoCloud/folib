package com.veadan.folib.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author veadan
 */
@Immutable
public class CronJobStringTypeField
        extends CronJobTypeField
{

    public CronJobStringTypeField()
    {
        this(null);
    }

    public CronJobStringTypeField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getValue()
    {
        return String.class.getSimpleName().toLowerCase();
    }
}
