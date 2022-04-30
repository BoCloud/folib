package com.veadan.folib.storage.metadata.maven;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;

import org.springframework.stereotype.Component;

@Component
public class RefreshMetadataExpirationStrategy
        implements MetadataExpirationStrategy
{

    @Override
    public Decision decide(RepositoryPath repositoryPath)
            throws IOException
    {
        return Decision.EXPIRED;
    }
}
