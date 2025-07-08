package com.folib.validation.cron.type;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class StringCronTaskConfigurationFormFieldTypeValidator
        implements CronTaskConfigurationFormFieldTypeValidator
{

    @Override
    public boolean isValid(String value)
    {
        return true;
    }

    @Override
    public boolean supports(String type)
    {
        return String.class.getSimpleName().toLowerCase().equals(type);
    }

}
