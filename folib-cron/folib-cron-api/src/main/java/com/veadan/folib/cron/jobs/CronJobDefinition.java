package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.jobs.fields.CronJobField;

import java.util.Set;

import org.springframework.util.Assert;

/**
 * @author Przemyslaw Fusik
 */
public class CronJobDefinition
{

    private String jobClass;

    private String name;

    private Set<CronJobField> fields;

    public String getJobClass()
    {
        return jobClass;
    }

    public String getName()
    {
        return name;
    }

    public Set<CronJobField> getFields()
    {
        return fields;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        CronJobDefinition that = (CronJobDefinition) o;

        return jobClass.equals(that.jobClass);
    }

    @Override
    public int hashCode()
    {
        return jobClass.hashCode();
    }

    private CronJobDefinition(Builder builder)
    {
        Assert.notNull(builder.jobClass, "jobClass should not be null");
        jobClass = builder.jobClass;
        name = builder.name;
        fields = builder.fields;
    }

    public static Builder newBuilder()
    {
        return new Builder();
    }


    public static final class Builder
    {

        private String jobClass;
        private String name;
        private String description;
        private Set<CronJobField> fields;

        private Builder()
        {
        }

        public Builder jobClass(String val)
        {
            jobClass = val;
            return this;
        }

        public Builder name(String val)
        {
            name = val;
            return this;
        }

        public Builder description(String val)
        {
            description = val;
            return this;
        }

        public Builder fields(Set<CronJobField> val)
        {
            fields = val;
            return this;
        }

        public CronJobDefinition build()
        {
            return new CronJobDefinition(this);
        }
    }
}
