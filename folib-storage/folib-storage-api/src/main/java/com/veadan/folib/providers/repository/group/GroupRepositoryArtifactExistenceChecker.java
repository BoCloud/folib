package com.veadan.folib.providers.repository.group;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import org.apache.commons.lang3.mutable.MutableBoolean;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class GroupRepositoryArtifactExistenceChecker
{

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    public boolean artifactExistsInTheGroupRepositorySubTree(final Repository groupRepository,
                                                             final RepositoryPath repositoryPath)
            throws IOException
    {
        return artifactExistsInTheGroupRepositorySubTree(groupRepository, repositoryPath, new HashMap<>());
    }

    public boolean artifactExistsInTheGroupRepositorySubTree(final Repository groupRepository,
                                                             final RepositoryPath repositoryPath,
                                                             final Map<String, MutableBoolean> repositoryArtifactExistence)
            throws IOException
    {
        for (final String maybeStorageAndRepositoryId : groupRepository.getGroupRepositories())
        {
            final String subStorageId = getStorageId(groupRepository, maybeStorageAndRepositoryId);
            final String subRepositoryId = getRepositoryId(maybeStorageAndRepositoryId);
            final Repository subRepository = getRepository(subStorageId, subRepositoryId);

            final String storageAndRepositoryId = subStorageId + ":" + subRepositoryId;
            repositoryArtifactExistence.putIfAbsent(storageAndRepositoryId, new MutableBoolean());
            if (repositoryArtifactExistence.get(storageAndRepositoryId).isTrue())
            {
                return true;
            }

            if (subRepository.isGroupRepository())
            {
                boolean artifactExistence = artifactExistsInTheGroupRepositorySubTree(subRepository,
                                                                                      repositoryPath,
                                                                                      repositoryArtifactExistence);
                if (artifactExistence)
                {
                    repositoryArtifactExistence.get(storageAndRepositoryId).setTrue();
                    return true;
                }
            }
            else
            {
                final LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(subRepository.getLayout());
                RepositoryPath subRepositoryPath = repositoryPathResolver.resolve(subRepository, repositoryPath.relativize());
                
                if (RepositoryFiles.artifactExists(subRepositoryPath))
                {
                    repositoryArtifactExistence.get(storageAndRepositoryId).setTrue();
                    return true;
                }
            }
        }
        return false;
    }

    private Repository getRepository(final String subStorageId,
                                     final String subRepositoryId)
    {
        return configurationManager.getConfiguration()
                                   .getStorage(subStorageId)
                                   .getRepository(subRepositoryId);
    }

    private String getRepositoryId(final String maybeStorageAndRepositoryId)
    {
        return ConfigurationUtils.getRepositoryId(maybeStorageAndRepositoryId);
    }

    private String getStorageId(final Repository groupRepository,
                                final String maybeStorageAndRepositoryId)
    {
        return ConfigurationUtils.getStorageId(groupRepository.getStorage().getId(),
                                               maybeStorageAndRepositoryId);
    }

}
