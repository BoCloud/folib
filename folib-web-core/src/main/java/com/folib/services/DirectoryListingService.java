package com.folib.services;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

import com.folib.providers.io.RepositoryPath;
import com.folib.domain.DirectoryListing;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

public interface DirectoryListingService
{

    DirectoryListing fromStorages(Map<String, ? extends Storage> storages) throws IOException;

    DirectoryListing fromRepositories(Map<String, ? extends Repository> repositories) throws IOException;

    DirectoryListing fromRepositoryPath(RepositoryPath path)
        throws IOException;

    DirectoryListing fromGroupRepositoryPath(Repository repository, RepositoryPath path)
            throws IOException;
    
    DirectoryListing fromPath(Path root, Path path)
            throws IOException;

}
