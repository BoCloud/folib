package com.veadan.folib.providers.repository.proxied;

import java.io.IOException;
import java.util.*;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import org.apache.commons.collections4.CollectionUtils;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author veadan
 */
@Component
public class LocalStorageProxyRepositoryExpiredArtifactsCleaner
{

    private final Logger logger = LoggerFactory.getLogger(LocalStorageProxyRepositoryExpiredArtifactsCleaner.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactRepository artifactEntityRepository;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Transactional
    public void cleanup(final Integer lastAccessedTimeInDays,
                        final Long minSizeInBytes)
            throws IOException
    {
        final Page<Artifact> artifactEntries = artifactEntityRepository.findMatching(lastAccessedTimeInDays, minSizeInBytes,
                                                                                     PageRequest.of(0, 1));
        List<Artifact> artifactsToDelete = filterAccessibleProxiedArtifacts(artifactEntries.toList());
        if (artifactsToDelete.isEmpty())
        {
            return;
        }

        logger.info("Cleaning artifacts {}", artifactsToDelete.size());
        deleteFromStorage(artifactsToDelete);
    }

    private List<Artifact> filterAccessibleProxiedArtifacts(final List<Artifact> artifactEntries)
    {
        if (CollectionUtils.isEmpty(artifactEntries))
        {
            return Collections.emptyList();
        }
        
        List<Artifact> result = new ArrayList<>();
        for (final Iterator<Artifact> it = artifactEntries.iterator(); it.hasNext(); )
        {
            final Artifact artifactEntry = it.next();
            final Storage storage = configurationManager.getConfiguration().getStorage(artifactEntry.getStorageId());
            if (Objects.isNull(storage)) {
                result.add(artifactEntry);
                continue;
            }
            final Repository repository = storage.getRepository(artifactEntry.getRepositoryId());
            if (Objects.isNull(repository)) {
                result.add(artifactEntry);
                continue;
            }
            if (!repository.isProxyRepository())
            {
                continue;
            }
            final RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null)
            {
                logger.warn("Repository {} is not associated with remote repository", repository.getId());
                continue;
            }
            if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository))
            {
                logger.warn("Remote repository {} is down. Artifacts won't be cleaned up.", remoteRepository.getUrl());
                continue;
            }
            
            result.add(artifactEntry);
        }
        
        return result;
    }

    private void deleteFromStorage(final List<Artifact> artifactEntries)
            throws IOException
    {
        for (final Artifact artifactEntry : artifactEntries)
        {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), artifactEntry.getArtifactPath());
            artifactManagementService.delete(repositoryPath, true);
        }
    }

}
