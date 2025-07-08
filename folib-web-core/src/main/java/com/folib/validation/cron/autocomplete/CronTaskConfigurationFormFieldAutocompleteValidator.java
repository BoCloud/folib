package com.folib.validation.cron.autocomplete;

/**
 * @author veadan
 */
public interface CronTaskConfigurationFormFieldAutocompleteValidator
{

    boolean isValid(String value);

    boolean supports(String name);
}
