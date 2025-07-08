package com.folib.validation.cron.type;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class BooleanCronTaskConfigurationFormFieldTypeValidator
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
        return "true".equals(value) || "false".equals(value);
    }

    @Override
    public boolean supports(String type)
    {
        return boolean.class.getSimpleName().equals(type);
    }
}
