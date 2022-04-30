package com.veadan.folib.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class CronJobBooleanTypeField
        extends CronJobTypeField
{

    public CronJobBooleanTypeField()
    {
        this(null);
    }

    public CronJobBooleanTypeField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getValue()
    {
        return boolean.class.getSimpleName();
    }
}
