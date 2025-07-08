package com.folib.validation.configuration;

import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;


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
