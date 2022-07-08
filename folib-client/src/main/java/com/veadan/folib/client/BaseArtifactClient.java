package com.veadan.folib.client;

import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements basic API for artifact processing. Subclasses may specify particular remote method implementations.
 *
 */
public abstract class BaseArtifactClient
        implements IArtifactClient
{

    protected final Logger logger = LoggerFactory.getLogger(getClass().getName());

    public InputStream getResource(String path)
            throws ArtifactTransportException,
                   IOException
    {
        return getResource(path, 0);
    }

    protected String escapeUrl(String path)
    {
        String baseUrl = getContextBaseUrl() + (getContextBaseUrl().endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);

        return baseUrl + p;
    }

    protected abstract void put(InputStream is,
                                String url,
                                String fileName,
                                String mediaType)
            throws ArtifactOperationException;
}
