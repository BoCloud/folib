package com.veadan.folib.job.cron.jobs.fields;

import javax.annotation.concurrent.Immutable;

/**
 * @author veadan
 */
@Immutable
public class CronJobAliasNamedField
        extends CronJobField {

    private final String aliasName;

    public CronJobAliasNamedField(String aliasName) {
        this(null, aliasName);
    }

    public CronJobAliasNamedField(CronJobField field, String aliasName) {
        super(field);
        this.aliasName = aliasName;
    }

    @Override
    public String getKey() {
        return "aliasName";
    }

    @Override
    public String getValue() {
        return aliasName;
    }

    @Override
    public String getAliasName() {
        return getValue();
    }
}
