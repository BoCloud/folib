package com.veadan.folib.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class CronJobOptionalField
        extends CronJobField
{

    public CronJobOptionalField()
    {
        this(null);
    }

    public CronJobOptionalField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getKey()
    {
        return "required";
    }

    @Override
    public String getValue()
    {
        return String.valueOf(false);
    }
}
