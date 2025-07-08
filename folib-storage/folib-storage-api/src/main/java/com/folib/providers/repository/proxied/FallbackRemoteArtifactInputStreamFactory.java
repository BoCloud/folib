package com.folib.providers.repository.proxied;

import com.folib.providers.io.RepositoryPath;

import java.io.InputStream;
import java.util.function.Function;

/**
 * @author veadan
 * @date 1/15/2024 10:31
 */
public abstract class FallbackRemoteArtifactInputStreamFactory {

    protected abstract Function<Exception, InputStream> getFallbackRemoteArtifactInputStream(RepositoryPath repositoryPath);

    protected abstract String getLayout();
}
