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
public class CronTaskConfigurationFormFieldRepositoryIdAutocompleteValidator
        implements CronTaskConfigurationFormFieldAutocompleteValidator
{

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public boolean isValid(String value)
    {
        Configuration configuration = configurationManagementService.getConfiguration();
        Map<String, Storage> storages = configuration.getStorages();
        return storages.keySet()
                       .stream()
                       .filter(sId -> configuration.getStorage(sId).getRepositories().keySet().contains(value))
                       .findFirst()
                       .isPresent();
    }

    @Override
    public boolean supports(String name)
    {
        return "repositoryId".equals(name);
    }
}
