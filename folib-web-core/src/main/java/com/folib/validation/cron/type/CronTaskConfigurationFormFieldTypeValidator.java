package com.folib.validation.cron.type;

/**
 * @author veadan
 */
public interface CronTaskConfigurationFormFieldTypeValidator
{

    boolean isValid(String value);

    boolean supports(String type);
}
