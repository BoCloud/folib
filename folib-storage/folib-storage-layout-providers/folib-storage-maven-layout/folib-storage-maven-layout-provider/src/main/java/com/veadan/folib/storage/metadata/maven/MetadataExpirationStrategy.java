package com.veadan.folib.storage.metadata.maven;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;

public interface MetadataExpirationStrategy
{

    enum Decision
    {
        UNDECIDED, EXPIRED, USABLE
    }

    Decision decide(final RepositoryPath repositoryPath) throws IOException;
}
