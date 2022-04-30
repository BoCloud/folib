package com.veadan.folib.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author Przemyslaw Fusik
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
