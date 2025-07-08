package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;

/**
 * @author Veadan
 */
public abstract class AbstractRepositoryManagementStrategy
        implements RepositoryManagementStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractRepositoryManagementStrategy.class);

    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Lazy
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
