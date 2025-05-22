package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.artifact.locator.ArtifactDirectoryLocator;
import com.veadan.folib.artifact.locator.handlers.ArtifactLocationGenerateChecksumOperation;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.services.ChecksumService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Kate Novik.
 */
@Component
public class ChecksumServiceImpl
        implements ChecksumService
{
    private final Logger logger = LoggerFactory.getLogger(ChecksumServiceImpl.class);

    @Inject
    private ConfigurationManager configurationManager;
    
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public void regenerateChecksum(String storageId,
                                   String repositoryId,
                                   String basePath,
                                   String lastModifiedTime,
                                   boolean forceRegeneration)
        throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        if (Objects.isNull(storage)) {
            logger.warn("Storage [{}] not found", storageId);
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            logger.warn("Repository [{}] [{}] not found", storageId, repositoryId);
            return;
        }
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
        if (layoutProvider == null)
        {
            logger.warn("Trying to regenerate checksum for repository {} but layoutProvider was not found in registry {} ",
                        repository.getId(), repository.getLayout());
            return;
        }
        
        
        RepositoryPath repositoryBasePath = Optional.ofNullable(basePath)
                                                    .map(p -> repositoryPathResolver.resolve(repository, basePath))
                                                    .orElseGet(() -> repositoryPathResolver.resolve(repository));
        
        ArtifactLocationGenerateChecksumOperation operation = new ArtifactLocationGenerateChecksumOperation();
        operation.setBasePath(repositoryBasePath);
        operation.setLastModifiedTime(lastModifiedTime);
        operation.setForceRegeneration(forceRegeneration);
        operation.execute(repositoryBasePath);
    }

    public Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }
}
