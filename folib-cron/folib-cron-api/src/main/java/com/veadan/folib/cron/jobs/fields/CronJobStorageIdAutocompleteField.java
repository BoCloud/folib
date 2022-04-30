package com.veadan.folib.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class CronJobStorageIdAutocompleteField
        extends CronJobAutocompleteField
{

    public CronJobStorageIdAutocompleteField()
    {
        this(null);
    }

    public CronJobStorageIdAutocompleteField(CronJobField field)
    {
        super(field);
    }

    @Override
    public String getValue()
    {
        return "storageId";
    }
}
