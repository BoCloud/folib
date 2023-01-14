package com.veadan.folib.services;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryStreamSupport;

/**
 * @author mtodorov
 */
public interface ArtifactResolutionService
{
    
    RepositoryStreamSupport.RepositoryInputStream getInputStream(RepositoryPath path)
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
}
