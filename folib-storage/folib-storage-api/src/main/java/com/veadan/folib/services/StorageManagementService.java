package com.veadan.folib.services;

import com.veadan.folib.storage.StorageDto;

import java.io.IOException;

/**
 * @author mtodorov
 */
public interface StorageManagementService
{

    void saveStorage(StorageDto storage)
            throws IOException;

    void removeStorage(String storageId)
            throws IOException;

}
