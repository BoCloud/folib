package com.folib.services.impl;

import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.artifact.locator.handlers.ArtifactLocationGenerateChecksumOperation;
import com.folib.configuration.Configuration;
import com.folib.services.ChecksumService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
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
