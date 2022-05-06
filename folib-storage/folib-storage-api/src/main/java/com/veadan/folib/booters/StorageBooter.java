package com.veadan.folib.booters;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.repository.group.GroupRepositorySetCollector;
import com.veadan.folib.repository.RepositoryManagementStrategyException;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryStatusEnum;
import com.veadan.folib.util.ThrowingConsumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.cp.lock.FencedLock;

/**
 * @author mtodorov
 */
public class StorageBooter
{

    private static final Logger logger = LoggerFactory.getLogger(StorageBooter.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    private GroupRepositorySetCollector groupRepositorySetCollector;

    @Inject
    private PropertiesBooter propertiesBooter;

    @Inject
    private HazelcastInstance hazelcastInstance;

    public StorageBooter()
    {
    }

    @PostConstruct
    public void initialize()
            throws IOException, RepositoryManagementStrategyException
    {
        FencedLock lock = hazelcastInstance.getCPSubsystem().getLock("StorageBooterLock");
        if (lock.tryLock())
        {
            try
            {
                Optional.of(System.getProperty("java.io.tmpdir"))
                        .map(Paths::get)
                        .filter(Path::isAbsolute)
                        .ifPresent(ThrowingConsumer.unchecked(Files::createDirectories));
                
                Configuration configuration = configurationManager.getConfiguration();
                initializeStorages(configuration.getStorages());
                Collection<Repository> repositories = getRepositoriesHierarchy(configuration.getStorages());
                if (!repositories.isEmpty())
                {
                    logger.info(" -> Initializing repositories...");
                }
                repositories.forEach(ThrowingConsumer.unchecked(this::initializeRepository));
            }
            finally
            {
                lock.unlock();
            }
        }
        else
        {
            logger.debug("Failed to initialize the repositories. Another JVM may have already done this.");
        }
    }


    private void initializeStorages(final Map<String, Storage> storages)
            throws IOException
    {
        logger.info("Running folib storage booter...");
        logger.info(" -> Creating storage directory skeleton...");

        for (Map.Entry<String, Storage> stringStorageEntry : storages.entrySet())
        {
            initializeStorage(stringStorageEntry.getValue());
        }

    }

    private void initializeStorage(Storage storage)
            throws IOException
    {
        logger.info("  * Initializing {}...", storage.getId());
    }

    private void initializeRepository(Repository repository)
            throws IOException, RepositoryManagementStrategyException
    {
        logger.info("  * Initializing {}:{}...", repository.getStorage().getId(), repository.getId());

        if (layoutProviderRegistry.getProvider(repository.getLayout()) == null)
        {
            logger.error("Failed to resolve layout [{}] for repository [{}].",
                         repository.getLayout(),
                         repository.getId());
            return;
        }

        repositoryManagementService.createRepository(repository.getStorage().getId(), repository.getId());

        if (RepositoryStatusEnum.IN_SERVICE.getStatus().equals(repository.getStatus()))
        {
            repositoryManagementService.putInService(repository.getStorage().getId(), repository.getId());
        }
    }

    private Collection<Repository> getRepositoriesHierarchy(final Map<String, Storage> storages)
    {
        final Map<String, Repository> repositoriesHierarchy = new LinkedHashMap<>();
        for (final Storage storage : storages.values())
        {
            for (final Repository repository : storage.getRepositories().values())
            {
                addRepositoriesByChildrenFirst(repositoriesHierarchy, repository);
            }
        }

        return repositoriesHierarchy.values();
    }

    private void addRepositoriesByChildrenFirst(final Map<String, Repository> repositoriesHierarchy,
                                                final Repository repository)
    {
        if (!repository.isGroupRepository())
        {
            repositoriesHierarchy.putIfAbsent(repository.getId(), repository);

            return;
        }

        groupRepositorySetCollector.collect(repository, true)
                                   .stream().forEach(r -> addRepositoriesByChildrenFirst(repositoriesHierarchy, r));

        repositoriesHierarchy.putIfAbsent(repository.getId(), repository);
    }

    public RepositoryManagementService getRepositoryManagementService()
    {
        return repositoryManagementService;
    }

    public void setRepositoryManagementService(RepositoryManagementService repositoryManagementService)
    {
        this.repositoryManagementService = repositoryManagementService;
    }

}
