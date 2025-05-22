package com.veadan.folib.storage.indexing;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public abstract class AbstractRepositoryIndexCreator
        implements RepositoryIndexCreator
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    protected RepositoryPathLock repositoryPathLock;

    @Override
    public RepositoryPath apply(Repository repository)
            throws IOException
    {
        final RepositoryPath repositoryIndexDirectoryPath = getRepositoryIndexDirectoryPathResolver().resolve(
                repository);

        if (repositoryPathLock.lock(repositoryIndexDirectoryPath))
        {
            try (final RepositoryCloseableIndexingContext indexingContext = getRepositoryIndexingContextFactory().create(
                    repository))
            {
                onIndexingContextCreated(repositoryIndexDirectoryPath, indexingContext);
            }
            finally
            {
                repositoryPathLock.unLock(repositoryIndexDirectoryPath);
            }
        }
        else
        {
            throw new IndexLockedException(String.format("Index of repository [%s:%s] is currently locked.",
                                                         repository.getStorage().getId(), repository.getId()));
        }

        return repositoryIndexDirectoryPath;
    }

    protected abstract void onIndexingContextCreated(RepositoryPath repositoryIndexDirectoryPath,
                                                     RepositoryCloseableIndexingContext indexingContext)
            throws IOException;

    protected abstract RepositoryIndexingContextFactory getRepositoryIndexingContextFactory();

    protected abstract RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver();
}
