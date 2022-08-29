package com.veadan.folib.services;

import com.veadan.folib.storage.StorageDto;

import java.io.IOException;

/**
 * @author mtodorov
 */
public interface StorageManagementService
{

    void createStorage(StorageDto storage)
            throws IOException;;

    void updateStorage(StorageDto storage)
            throws IOException;

    void removeStorage(String storageId)
            throws IOException;

}
