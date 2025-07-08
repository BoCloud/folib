package com.folib.storage.indexing.local;

import com.folib.storage.indexing.AbstractRepositoryIndexingContextFactory;
import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;

import jakarta.inject.Inject;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.LOCAL)
public class RepositoryLocalIndexingContextFactory
        extends AbstractRepositoryIndexingContextFactory
{

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver;

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return indexDirectoryPathResolver;
    }
}
