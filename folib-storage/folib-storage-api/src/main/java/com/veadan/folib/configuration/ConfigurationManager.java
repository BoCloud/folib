package com.veadan.folib.configuration;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.net.URI;

import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author mtodorov
 */
@Component
public class ConfigurationManager implements StoragesConfigurationManager
{

    @Inject
    @Lazy
    private ConfigurationManagementService configurationService;

    public Repository getRepository(String storageAndRepositoryId)
    {
        String[] elements = storageAndRepositoryId.split(":");
        String storageId = elements[0];
        String repositoryId = elements[1];

        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Repository getRepository(String storageId,
                                    String repositoryId)
    {
        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Storage getStorage(String storageId)
    {
        return getConfiguration().getStorage(storageId);
    }
    
    public Configuration getConfiguration()
    {
        return configurationService.getConfiguration();
    }

    public URI getBaseUri()
    {
        try
        {
            return URI.create(getConfiguration().getBaseUrl());
        }
        catch (IllegalArgumentException e)
        {
            throw new InvalidConfigurationException(e);
        }
    }

    public Integer getSessionTimeoutSeconds() {
        return getConfiguration().getSessionConfiguration().getTimeoutSeconds();
    }
}
