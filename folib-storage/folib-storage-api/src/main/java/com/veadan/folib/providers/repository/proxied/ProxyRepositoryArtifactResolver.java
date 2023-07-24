package com.veadan.folib.providers.repository.proxied;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

import javax.inject.Inject;

import com.veadan.folib.client.RestArtifactResolver;
import com.veadan.folib.config.HelmRepoUtil;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ProxyRepositoryArtifactResolver
{
    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryArtifactResolver.class);

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RestArtifactResolverFactory restArtifactResolverFactory;
    
    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    private HelmRepoUtil helmRepoUtil;

    /**
     * This method has been developed to force fetch resource from remote.
     *
     * It should not contain any local / cache existence checks.
     *
     * Update this method carefully.
     */
    public RepositoryPath fetchRemoteResource(RepositoryPath repositoryPath)
        throws IOException
    {
        Repository repository = repositoryPath.getFileSystem().getRepository();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository))
        {
            logger.info("Remote repository '{}' is down.", remoteRepository.getUrl());

            return null;
        }

        RestArtifactResolver client = restArtifactResolverFactory.newInstance(remoteRepository,repositoryPath);
//        ReadWriteLock lockSource = repositoryPathLock.lock(repositoryPath, "remote-fetch");
//        Lock lock = lockSource.writeLock();
//        lock.lock();

        try (InputStream is = new BufferedInputStream(new ProxyRepositoryInputStream(client, repositoryPath)))
        {
            return doFetch(repositoryPath, is);
        }
//        finally
//        {
//            lock.unlock();
//        }
    }

    private RepositoryPath doFetch(RepositoryPath repositoryPath,
                                   InputStream is)
        throws IOException
    {
        //We need this to force initialize lazy connection to remote repository.
        int available = is.available();
        logger.info("Got [{}] available bytes for [{}].", available, repositoryPath);
        
        RepositoryPath result = onSuccessfulProxyRepositoryResponse(is, repositoryPath);
        if (RepositoryFiles.isArtifact(repositoryPath))
        {
            artifactEventListenerRegistry.dispatchArtifactFetchedFromRemoteEvent(result);
        }
        return result;
    }

    protected RepositoryPath onSuccessfulProxyRepositoryResponse(InputStream is,
                                                                 RepositoryPath repositoryPath)
            throws IOException {
        boolean indexFlage = repositoryPath.getRepository().getLayout().equalsIgnoreCase("helm")
                && repositoryPath.toString().endsWith("index.yaml");
        artifactManagementService.store(repositoryPath, is);
        // helm 代理修改索引
        if (indexFlage) {
            helmRepoUtil.reloadIndex(repositoryPath);
            logger.info("重新加载 heml indx");
        }
        // TODO: Add a policy for validating the checksums of downloaded artifacts
        // TODO: Validate the local checksum against the remote's checksums    徐新平
        // Serve the downloaded artifact
        return repositoryPath;
    }

}
