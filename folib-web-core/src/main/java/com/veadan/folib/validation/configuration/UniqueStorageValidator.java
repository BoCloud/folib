package com.veadan.folib.validation.configuration;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;

import javax.inject.Inject;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

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
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        return storage == null;
    }

}
