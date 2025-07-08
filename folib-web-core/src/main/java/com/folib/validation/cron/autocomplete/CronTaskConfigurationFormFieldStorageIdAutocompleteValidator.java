package com.folib.validation.cron.autocomplete;

import com.folib.configuration.Configuration;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;

import javax.inject.Inject;
import java.util.Map;

import org.springframework.stereotype.Component;

/**
 * @author veadan
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
