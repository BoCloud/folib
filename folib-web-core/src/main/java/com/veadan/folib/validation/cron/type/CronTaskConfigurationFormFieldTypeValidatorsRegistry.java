package com.veadan.folib.validation.cron.type;

import javax.inject.Inject;
import java.util.List;

import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class CronTaskConfigurationFormFieldTypeValidatorsRegistry
{

    @Inject
    private List<CronTaskConfigurationFormFieldTypeValidator> validators;

    public CronTaskConfigurationFormFieldTypeValidator get(String type)
    {
        return validators.stream()
                         .filter(v -> v.supports(type))
                         .findFirst()
                         .orElseThrow(
                                 () -> new IllegalArgumentException(String.format("Type %s not recognized", type)));
    }
}
