package com.veadan.folib.validation.cron.type;

/**
 * @author Przemyslaw Fusik
 */
public interface CronTaskConfigurationFormFieldTypeValidator
{

    boolean isValid(String value);

    boolean supports(String type);
}
