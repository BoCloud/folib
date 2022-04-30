package com.veadan.folib.services.impl;

import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.event.Event;
import com.veadan.folib.event.RepositoryBasedEvent;
import com.veadan.folib.event.repository.RepositoryEvent;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.event.repository.RepositoryEventTypeEnum;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.resource.ArtifactOperationsValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author mtodorov
 */
@Component("repositoryManagementService")
public class RepositoryManagementServiceImpl
        implements RepositoryManagementService
{

    private static final Logger logger = LoggerFactory.getLogger(RepositoryManagementServiceImpl.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private ArtifactOperationsValidator artifactOperationsValidator;

    @Inject
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;


    @Override
    public void createRepository(String storageId,
                                 String repositoryId)
            throws IOException, RepositoryManagementStrategyException
    {
        LayoutProvider provider = getLayoutProvider(storageId, repositoryId);
        if (provider != null)
        {
            provider.getRepositoryManagementStrategy().createRepository(storageId, repositoryId);
        }
        else
        {
            Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryId);

            logger.warn("Layout provider '{}' could not be resolved. " +
                        "Using generic implementation instead.",
                        repository.getLayout());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);

            if (!Files.exists(repositoryPath))
            {
                logger.info("Creating directories for [{}/{}]...", repository.getStorage().getId(), repository.getId());
                repositoryPath.getFileSystem().createRootDirectory();
            }
        }

        RepositoryEvent event = new RepositoryEvent(storageId,
                                                    repositoryId,
                                                    RepositoryEventTypeEnum.EVENT_REPOSITORY_CREATED.getType());

        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void removeRepository(String storageId,
                                 String repositoryId)
            throws IOException
    {
        LayoutProvider provider = getLayoutProvider(storageId, repositoryId);
        provider.getRepositoryManagementStrategy().removeRepository(storageId, repositoryId);

        RepositoryEvent event = new RepositoryEvent(storageId,
                                                    repositoryId,
                                                    RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETED.getType());

        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void deleteTrash(String storageId, String repositoryId)
            throws IOException
    {
        artifactOperationsValidator.checkStorageExists(storageId);
        artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);

        try
        {
            final Storage storage = getStorage(storageId);
            final Repository repository = storage.getRepository(repositoryId);

            artifactOperationsValidator.checkAllowsDeletion(repository);

            
            RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
            RepositoryFiles.deleteTrash(repositoryPath);

            RepositoryEvent event = new RepositoryEvent(storageId,
                                                        repositoryId,
                                                        RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH.getType());

            repositoryEventListenerRegistry.dispatchEvent(event);
        }
        catch (IOException e)
        {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void deleteTrash()
            throws ArtifactStorageException
    {
        try
        {
            for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet())
            {
                Storage storage = entry.getValue();

                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values())
                {
                    if (repository.allowsDeletion())
                    {
                        logger.debug("Emptying trash for repository {}...", repository.getId());

                        deleteTrash(repository.getStorage().getId(), repository.getId());;
                    }
                    else
                    {
                        logger.warn("Repository {} does not support removal of trash.", repository.getId());
                    }
                }
            }

            int type = RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH_FOR_ALL_REPOSITORIES.getType();
            RepositoryEvent event = new RepositoryEvent(null, null, type);

            repositoryEventListenerRegistry.dispatchEvent(event);
        }
        catch (IOException e)
        {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undelete(RepositoryPath repositoryPath)
            throws IOException
    {
        artifactOperationsValidator.validate(repositoryPath);

        final Repository repository = repositoryPath.getRepository();
        
        artifactOperationsValidator.checkAllowsDeletion(repository);

        try
        {
            RepositoryFiles.undelete(repositoryPath);

            int type = RepositoryEventTypeEnum.EVENT_REPOSITORY_EMTPY_TRASH_FOR_ALL_REPOSITORIES.getType();
            Event event = new RepositoryBasedEvent<>(repositoryPath, type);

            repositoryEventListenerRegistry.dispatchEvent(event);
        }
        catch (IOException e)
        {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undeleteTrash(String storageId, String repositoryId)
            throws IOException
    {
        artifactOperationsValidator.checkStorageExists(storageId);
        artifactOperationsValidator.checkRepositoryExists(storageId, repositoryId);

        try
        {
            final Storage storage = getStorage(storageId);
            final Repository repository = storage.getRepository(repositoryId);

            if (repository.isTrashEnabled())
            {
                RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
                RepositoryFiles.undelete(repositoryPath);

                RepositoryEvent event = new RepositoryEvent(storageId,
                                                            repositoryId,
                                                            RepositoryEventTypeEnum.EVENT_REPOSITORY_UNDELETE_TRASH
                                                                                   .getType());

                repositoryEventListenerRegistry.dispatchEvent(event);
            }
        }
        catch (IOException e)
        {
            throw new ArtifactStorageException(e.getMessage(), e);
        }
    }

    @Override
    public void undeleteTrash()
            throws IOException
    {
        
        for (Map.Entry<String, Storage> entry : getConfiguration().getStorages().entrySet())
        {
            Storage storage = entry.getValue();

            final Map<String, ? extends Repository> repositories = storage.getRepositories();
            for (Repository repository : repositories.values())
            {
                final String storageId = storage.getId();
                final String repositoryId = repository.getId();

                try
                {
                    if (repository.isTrashEnabled())
                    {
                        RootRepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
                        RepositoryFiles.undelete(repositoryPath);
                    }
                }
                catch (IOException e)
                {
                    throw new ArtifactStorageException( "Unable to undelete trash for storage " + storageId + " in repository " +
                            repositoryId, e);
                }
            }
        }
        
        RepositoryEvent event = new RepositoryEvent(null,
                                                    null,
                                                    RepositoryEventTypeEnum.EVENT_REPOSITORY_UNDELETE_TRASH_FOR_ALL_REPOSITORIES
                                                                           .getType());
        repositoryEventListenerRegistry.dispatchEvent(event);
    }

    @Override
    public void putInService(String storageId,
                             String repositoryId) throws IOException
    {
        configurationManagementService.putInService(storageId, repositoryId);
    }

    @Override
    public void putOutOfService(String storageId,
                                String repositoryId) throws IOException
    {
        configurationManagementService.putOutOfService(storageId, repositoryId);
    }

    private LayoutProvider getLayoutProvider(String storageId,
                                             String repositoryId)
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        return layoutProviderRegistry.getProvider(repository.getLayout());
    }

    @Override
    public Storage getStorage(String storageId)
    {
        return getConfiguration().getStorages().get(storageId);
    }

    public Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

}
