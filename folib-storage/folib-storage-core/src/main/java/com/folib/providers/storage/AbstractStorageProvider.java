package com.folib.providers.storage;

import org.springframework.context.annotation.Lazy;

import jakarta.inject.Inject;

/**
 * @author Veadan
 */
public abstract class AbstractStorageProvider
        implements StorageProvider
{

    @Inject
    @Lazy
    private StorageProviderRegistry storageProviderRegistry;


    public StorageProviderRegistry getStorageProviderRegistry()
    {
        return storageProviderRegistry;
    }

    public void setStorageProviderRegistry(StorageProviderRegistry storageProviderRegistry)
    {
        this.storageProviderRegistry = storageProviderRegistry;
    }

}
