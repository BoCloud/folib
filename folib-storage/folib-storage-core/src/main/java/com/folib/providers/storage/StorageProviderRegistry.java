package com.folib.providers.storage;

import com.folib.providers.AbstractMappedProviderRegistry;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class StorageProviderRegistry
        extends AbstractMappedProviderRegistry<StorageProvider>
{

    private static final Logger logger = LoggerFactory.getLogger(StorageProviderRegistry.class);

    @Inject
    private List<StorageProvider> storageProviders;


    @Override
    @PostConstruct
    public void initialize()
    {
        storageProviders.forEach(sp -> addProvider(sp.getAlias(), sp));

        logger.info("Initialized the storage provider registry.");
    }

}
