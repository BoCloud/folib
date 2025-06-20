package com.veadan.folib.job.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author veadan
 */
@Immutable
public class CronJobRepositoryIdAutocompleteField
        extends CronJobAutocompleteField
{
    public CronJobRepositoryIdAutocompleteField()
    {
        this(null);
    }

    public CronJobRepositoryIdAutocompleteField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getValue()
    {
        return "repositoryId";
    }
}
