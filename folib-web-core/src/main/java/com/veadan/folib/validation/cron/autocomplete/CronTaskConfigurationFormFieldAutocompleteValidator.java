package com.veadan.folib.validation.cron.autocomplete;

/**
 * @author Przemyslaw Fusik
 */
public interface CronTaskConfigurationFormFieldAutocompleteValidator
{

    boolean isValid(String value);

    boolean supports(String name);
}
