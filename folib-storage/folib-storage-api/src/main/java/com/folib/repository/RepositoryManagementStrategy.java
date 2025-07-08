package com.folib.repository;

import com.folib.storage.repository.Repository;

import java.io.IOException;

/**
 * @author Veadan
 */
public interface RepositoryManagementStrategy
{

    void createRepository(String storageId,
                          String repositoryId)
            throws IOException, RepositoryManagementStrategyException;

    void createRepositoryStructure(Repository repository)
            throws IOException;

    void removeRepository(String storageId,
                          String repositoryId)
            throws IOException;

    void removeDirectoryStructure(String storageId,
                                  String repositoryId)
            throws IOException;

}
