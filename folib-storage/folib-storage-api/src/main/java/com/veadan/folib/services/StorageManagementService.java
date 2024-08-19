package com.veadan.folib.services;

import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    void handleStorageProvider(StorageDto storage) throws IOException;

    void syncYamlStorageUsers(Collection<Storage> values);

    void getStorageUsers(List<Storage> storages);
    Map<String, Set<String>> getStorageUser(Set<String> storageIds);
}
