package com.veadan.folib.configuration;

import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

public interface StoragesConfigurationManager
{

    Repository getRepository(String storageAndRepositoryId);

    Repository getRepository(String storageId,
                             String repositoryId);

    Storage getStorage(String storageId);

}
