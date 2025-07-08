package com.folib.job.cron.jobs.fields;

/**
 * @author veadan
 */
public abstract class CronJobAutocompleteField
        extends CronJobField
{

    public CronJobAutocompleteField()
    {
        this(null);
    }

    public CronJobAutocompleteField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getKey()
    {
        return "autocomplete";
    }

    @Override
    public String getAutocompleteValue()
    {
        return getValue();
    }
}
