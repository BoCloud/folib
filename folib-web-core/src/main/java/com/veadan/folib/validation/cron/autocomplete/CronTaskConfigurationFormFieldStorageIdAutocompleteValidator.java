package com.veadan.folib.validation.cron.autocomplete;

import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;

import javax.inject.Inject;
import java.util.Map;

import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class CronTaskConfigurationFormFieldStorageIdAutocompleteValidator
        implements CronTaskConfigurationFormFieldAutocompleteValidator
{

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public boolean isValid(String value)
    {
        Configuration configuration = configurationManagementService.getConfiguration();
        Map<String, Storage> storages = configuration.getStorages();
        return storages.keySet().contains(value);
    }

    @Override
    public boolean supports(String name)
    {
        return "storageId".equals(name);
    }
}
