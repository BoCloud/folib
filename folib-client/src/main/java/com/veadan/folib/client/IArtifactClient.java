package com.veadan.folib.client;

import java.io.IOException;
import java.io.InputStream;

/**
 * Artifact processing API.
 *
 */
public interface IArtifactClient    // named with I prefix because of existing ArtifactClient class in master branch
{
    String getContextBaseUrl();

    void deployFile(InputStream is,
                    String url,
                    String fileName)
            throws ArtifactOperationException;

    boolean pathExists(String path);

    InputStream getResource(String path)
            throws ArtifactTransportException,
                   IOException;

    InputStream getResource(String path,
                            long offset)
            throws ArtifactTransportException,
                   IOException;

    void deployMetadata(InputStream is,
                        String url,
                        String fileName)
            throws ArtifactOperationException;
}
