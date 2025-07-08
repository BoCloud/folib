package com.folib.configuration;

import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

public interface StoragesConfigurationManager
{

    Repository getRepository(String storageAndRepositoryId);

    Repository getRepository(String storageId,
                             String repositoryId);

    Storage getStorage(String storageId);

}
