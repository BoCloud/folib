package com.veadan.folib.validation.cron.type;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class IntCronTaskConfigurationFormFieldTypeValidator
        implements CronTaskConfigurationFormFieldTypeValidator
{

    @Override
    public boolean isValid(String value)
    {
        // value requirements is not a subject of this validator
        if (StringUtils.isBlank(value))
        {
            return true;
        }
        try
        {
            return Integer.valueOf(value) != null;
        }
        catch (NumberFormatException ex)
        {
            return false;
        }
    }

    @Override
    public boolean supports(String type)
    {
        return int.class.getSimpleName().equals(type);
    }
}
