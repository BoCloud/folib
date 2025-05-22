package com.veadan.folib.validation.configuration;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;

import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.collections4.map.CaseInsensitiveMap;


import java.util.Map;

/**
 * @author Veadan
 */
public class UniqueStorageValidator
        implements ConstraintValidator<UniqueStorage, String>
{

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public void initialize(UniqueStorage constraint)
    {
        // Empty method, not used.
    }

    @Override
    public boolean isValid(String storageId,
                           ConstraintValidatorContext context)
    {
        Map<String, Storage> storageMap = configurationManagementService.getConfiguration().getStorages();
        CaseInsensitiveMap<String, Storage> insensitiveMap = new CaseInsensitiveMap<>(storageMap);
        Storage storage = insensitiveMap.get(storageId);
        return storage == null;
    }

}
