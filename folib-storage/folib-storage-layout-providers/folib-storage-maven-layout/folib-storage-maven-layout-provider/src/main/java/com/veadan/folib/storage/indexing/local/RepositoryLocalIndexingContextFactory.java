package com.veadan.folib.storage.indexing.local;

import com.veadan.folib.storage.indexing.AbstractRepositoryIndexingContextFactory;
import com.veadan.folib.storage.indexing.IndexTypeEnum;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.veadan.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;

import javax.inject.Inject;

import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
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
