package com.veadan.folib.services.impl;

import java.io.IOException;

import javax.inject.Inject;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author mtodorov
 */
@Component("storageManagementService")
public class StorageManagementServiceImpl implements StorageManagementService
{

    protected static final Logger logger = LoggerFactory.getLogger(StorageManagementServiceImpl.class);

    @Inject
    private ConfigurationManagementService configurationManagementService;
    
    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Override
    public void updateStorage(StorageDto storage)
            throws IOException
    {
        configurationManagementService.updateStorage(storage);
    }

    @Override
    public void createStorage(StorageDto storage)
            throws IOException
    {
        configurationManagementService.createStorage(storage);
    }

    @Override
    public void removeStorage(String storageId)
            throws IOException
    {
        final Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        for (Repository repository : storage.getRepositories().values())
        {
            repositoryManagementService.removeRepository(storageId, repository.getId());
        }
    }

}

