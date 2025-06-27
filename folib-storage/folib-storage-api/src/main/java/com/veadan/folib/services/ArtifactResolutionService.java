package com.veadan.folib.services;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryStreamSupport;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

/**
 * @author veadan
 */
public interface ArtifactResolutionService {

    RepositoryStreamSupport.RepositoryInputStream getInputStream(RepositoryPath path)
            throws IOException;


    RepositoryStreamSupport.RepositoryStoreIndexInputStream getStoreIndexInputStream(RepositoryPath path)
            throws IOException;

    RepositoryStreamSupport.RepositoryOutputStream getOutputStream(RepositoryPath repositoryPath)
            throws IOException,
            NoSuchAlgorithmException;

    RepositoryPath resolvePath(String storageId,
                               String repositoryId,
                               String path)
            throws IOException;

    RepositoryPath resolvePath(String storageId,
                               String repositoryId,
                               String targetUrl,
                               String path)
            throws IOException;

    RepositoryPath resolvePath(RepositoryPath repositoryPath)
            throws IOException;
}
