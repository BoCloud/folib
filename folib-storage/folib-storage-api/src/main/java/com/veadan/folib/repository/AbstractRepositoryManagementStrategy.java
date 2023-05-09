package com.veadan.folib.repository;

import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Veadan
 */
public abstract class AbstractRepositoryManagementStrategy
        implements RepositoryManagementStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractRepositoryManagementStrategy.class);

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public void createRepository(String storageId,
                                 String repositoryId)
            throws IOException, RepositoryManagementStrategyException
    {
        Storage storage = getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        createRepositoryStructure(repository);
        createRepositoryInternal(storage, getRepository(storageId, repositoryId));
    }

    @Override
    public void createRepositoryStructure(final Repository repository)
            throws IOException
    {
        final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        if (!Files.exists(rootRepositoryPath))
        {
            rootRepositoryPath.getFileSystem().createRootDirectory();
        }

        final RepositoryPath trashRepositoryPath = rootRepositoryPath.resolve(LayoutFileSystem.TRASH);
        if (!Files.exists(trashRepositoryPath))
        {
            Files.createDirectories(trashRepositoryPath);
        }
    }

    protected void createRepositoryInternal(Storage storage,
                                            Repository repository)
            throws RepositoryManagementStrategyException
    {
        // override if needed
    }

    protected Storage getStorage(String storageId)
    {
        return getConfiguration().getStorage(storageId);
    }

    protected Repository getRepository(String storageId,
                                       String repositoryId)
    {
        return getStorage(storageId).getRepository(repositoryId);
    }

    @Override
    public void removeRepository(String storageId,
                                 String repositoryId)
            throws IOException
    {
        removeDirectoryStructure(storageId, repositoryId);
    }

    @Override
    public void removeDirectoryStructure(String storageId,
                                         String repositoryId)
            throws IOException
    {
        Repository repository = getRepository(storageId, repositoryId);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);

        if (Files.exists(repositoryPath))
        {
            Files.delete(repositoryPath);

            logger.info("Removed directory structure for repository '{}'.", repositoryPath);
        }
        else
        {
            throw new IOException(String.format("Failed to delete non-existing repository '%s'.", repositoryPath));
        }
        
        repositoryPath.getFileSystem().cleanupRootDirectory();
    }


    protected Configuration getConfiguration()
    {
        return configurationManagementService.getConfiguration();
    }

}
