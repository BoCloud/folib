package com.veadan.folib.storage.indexing.remote;

import com.veadan.folib.storage.indexing.AbstractRepositoryIndexingContextFactory;
import com.veadan.folib.storage.indexing.IndexTypeEnum;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.veadan.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;

import jakarta.inject.Inject;

import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.REMOTE)
public class RepositoryRemoteIndexingContextFactory
        extends AbstractRepositoryIndexingContextFactory
{

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver;

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return indexDirectoryPathResolver;
    }

    @Override
    protected String getRepositoryUrl(final Repository repository)
    {
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            logger.warn("Repository [{}:{}] was expected to have remote repository provided but was null.",
                        repository.getStorage().getId(), repository.getId());
            return null;

        }
        return remoteRepository.getUrl();
    }
}
