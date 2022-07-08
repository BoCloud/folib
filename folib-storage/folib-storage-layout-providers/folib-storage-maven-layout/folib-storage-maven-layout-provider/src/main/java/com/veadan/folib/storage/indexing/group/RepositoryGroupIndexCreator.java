package com.veadan.folib.storage.indexing.group;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.indexing.*;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.indexing.*;
import com.veadan.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.veadan.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Inject;
import java.io.IOException;
import java.util.concurrent.locks.Lock;

import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.store.SimpleFSDirectory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.GROUP)
public class RepositoryGroupIndexCreator
        extends AbstractRepositoryIndexCreator
{

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexDirectoryPathResolver localIndexDirectoryPathResolver;

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.REMOTE)
    private RepositoryIndexDirectoryPathResolver remoteIndexDirectoryPathResolver;

    @Inject
    @RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexingContextFactory indexingContextFactory;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    protected void onIndexingContextCreated(final RepositoryPath repositoryIndexDirectoryPath,
                                            final RepositoryCloseableIndexingContext indexingContext)
            throws IOException
    {

        indexingContext.purge();
        mergeSubrepositoryIndexes(indexingContext);
        IndexPacker.pack(repositoryIndexDirectoryPath, indexingContext);
    }

    private void mergeSubrepositoryIndexes(RepositoryCloseableIndexingContext indexingContext)
            throws IOException
    {
        final Repository repository = indexingContext.getRepositoryRaw();
        final Storage storage = repository.getStorage();

        for (final String storageAndRepositoryId : repository.getGroupRepositories())
        {
            final String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
            final String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);

            final RepositoryPath subRepositoryIndexDirectoryPath = getSubRepositoryIndexPath(sId, rId);

            final Lock lock = repositoryPathLock.lock(subRepositoryIndexDirectoryPath).readLock();
            lock.lock();
            try
            {
                try
                {
                    indexingContext.merge(new SimpleFSDirectory(subRepositoryIndexDirectoryPath));
                }
                catch (IndexNotFoundException ex)
                {
                    logger.warn("IndexNotFound in [{}]", subRepositoryIndexDirectoryPath, ex);
                }
            }
            finally
            {
                lock.unlock();
            }
        }
    }

    private RepositoryPath getSubRepositoryIndexPath(final String storageId,
                                                     final String repositoryId)
    {
        final Repository repository = configurationManager.getRepository(storageId, repositoryId);

        final RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver =
                repository.isProxyRepository() ? remoteIndexDirectoryPathResolver :
                localIndexDirectoryPathResolver;

        return indexDirectoryPathResolver.resolve(repository);
    }

    @Override
    protected RepositoryIndexingContextFactory getRepositoryIndexingContextFactory()
    {
        return indexingContextFactory;
    }

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return localIndexDirectoryPathResolver;
    }
}

