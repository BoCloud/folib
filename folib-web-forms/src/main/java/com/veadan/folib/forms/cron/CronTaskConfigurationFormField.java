package com.veadan.folib.forms.cron;

/**
 * @author Przemyslaw Fusik
 */
public class CronTaskConfigurationFormField
{

    private String name;

    private String value;

    CronTaskConfigurationFormField()
    {

    }

    private CronTaskConfigurationFormField(Builder builder)
    {
        setName(builder.name);
        setValue(builder.value);
    }

    public static Builder newBuilder()
    {
        return new Builder();
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getValue()
    {
        return value;
    }

    public void setValue(String value)
    {
        this.value = value;
    }


    public static final class Builder
    {

        private String name;
        private String value;

        private Builder()
        {
        }

        public Builder name(String val)
        {
            name = val;
            return this;
        }

        public Builder value(String val)
        {
            value = val;
            return this;
        }

        public CronTaskConfigurationFormField build()
        {
            return new CronTaskConfigurationFormField(this);
        }
    }
}
