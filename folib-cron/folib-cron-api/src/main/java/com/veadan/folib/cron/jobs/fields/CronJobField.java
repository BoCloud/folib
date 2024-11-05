package com.veadan.folib.cron.jobs.fields;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
@JsonSerialize(using = CronJobFieldJsonSerializer.class)
public abstract class CronJobField
{

    private final CronJobField field;

    public CronJobField(CronJobField field)
    {
        this.field = field;
    }

    public abstract String getKey();

    public abstract String getValue();

    protected CronJobField getField()
    {
        return field;
    }

    public boolean isRequired()
    {
        return field != null && field.isRequired();
    }

    public String getName()
    {
        Assert.notNull(field, () -> String.format("Field %s does not have name", field));
        return field.getName();
    }

    public String getAliasName()
    {
        return field.getAliasName();
    }

    public String getType()
    {
        Assert.notNull(field, () -> String.format("Field %s does not have type", field));
        return field.getType();
    }

    public String getAutocompleteValue()
    {
        if (field == null)
        {
            return null;
        }
        return field.getAutocompleteValue();
    }
}
