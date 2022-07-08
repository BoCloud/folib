package com.veadan.folib.validation.configuration;

import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;

import javax.inject.Inject;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

/**
 * @author Veadan
 */
public class StorageProviderValidator
        implements ConstraintValidator<StorageProviderValue, String>
{

    @Inject
    private StorageProviderRegistry storageProviderRegistry;


    @Override
    public void initialize(StorageProviderValue constraint)
    {
        // Empty method, not used.
    }

    @Override
    public boolean isValid(String alias,
                           ConstraintValidatorContext context)
    {
        final StorageProvider storageProvider = storageProviderRegistry.getProvider(alias);

        return storageProvider != null;
    }

}
