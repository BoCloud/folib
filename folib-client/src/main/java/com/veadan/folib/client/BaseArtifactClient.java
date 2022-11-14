package com.veadan.folib.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * Implements basic API for artifact processing. Subclasses may specify particular remote method implementations.
 * @author veadan
 */
public abstract class BaseArtifactClient
        implements IArtifactClient {

    protected final Logger logger = LoggerFactory.getLogger(getClass().getName());

    @Override
    public InputStream getResource(String path) {
        return getResource(path, 0);
    }

    protected String escapeUrl(String path) {
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
