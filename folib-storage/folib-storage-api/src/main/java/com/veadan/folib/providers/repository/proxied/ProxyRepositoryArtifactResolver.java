package com.veadan.folib.providers.repository.proxied;

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

import javax.inject.Inject;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.function.Function;

/**
 * @author veadan
 */
@Component
public class ProxyRepositoryArtifactResolver {
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
    @Inject
    private List<FallbackRemoteArtifactInputStreamFactory> fallbackRemoteArtifactInputStreamRegistry;

    /**
     * This method has been developed to force fetch resource from remote.
     * <p>
     * It should not contain any local / cache existence checks.
     * <p>
     * Update this method carefully.
     */
    public RepositoryPath fetchRemoteResource(RepositoryPath repositoryPath)
            throws IOException {
        Repository repository = repositoryPath.getFileSystem().getRepository();
        final RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            logger.debug("Remote repository '{}' is down.", remoteRepository.getUrl());
        }

        RestArtifactResolver client = restArtifactResolverFactory.newInstance(remoteRepository,repositoryPath);
        Function<Exception, InputStream> fallback = null;
        for (FallbackRemoteArtifactInputStreamFactory fallbackRemoteArtifactInputStreamFactory : fallbackRemoteArtifactInputStreamRegistry) {
            if (repositoryPath.getRepository().getLayout().equals(fallbackRemoteArtifactInputStreamFactory.getLayout())){
                fallback = fallbackRemoteArtifactInputStreamFactory.getFallbackRemoteArtifactInputStream(repositoryPath);
                break;
            }
        }
        InputStream inputStream = new ProxyRepositoryInputStream(client, repositoryPath);
        if (fallback != null) {
            inputStream = new FallbackRemoteArtifactInputStream(inputStream, fallback);
        }

        try (InputStream is = new BufferedInputStream(inputStream)) {
            return doFetch(repositoryPath, is);
        }
    }

    private RepositoryPath doFetch(RepositoryPath repositoryPath,
                                   InputStream is)
            throws IOException {
        //We need this to force initialize lazy connection to remote repository.
        int available = is.available();
        logger.info("Got [{}] available bytes for [{}].", available, repositoryPath);

        RepositoryPath result = onSuccessfulProxyRepositoryResponse(is, repositoryPath);
        if (RepositoryFiles.isArtifact(repositoryPath)) {
            artifactEventListenerRegistry.dispatchArtifactFetchedFromRemoteEvent(result);
        }
        return result;
    }

    protected RepositoryPath onSuccessfulProxyRepositoryResponse(InputStream is,
                                                                 RepositoryPath repositoryPath)
            throws IOException {
        artifactManagementService.store(repositoryPath, is);
        // helm 代理修改索引
        boolean indexFlag = repositoryPath.getRepository().getLayout().equalsIgnoreCase("helm")
                && repositoryPath.toString().endsWith("index.yaml");
        if (indexFlag) {
            helmRepoUtil.reloadIndex(repositoryPath);
            logger.info("Reload helm index");
        }
        // TODO: Add a policy for validating the checksums of downloaded artifacts
        // TODO: Validate the local checksum against the remote's checksums    徐新平
        // Serve the downloaded artifact
        return repositoryPath;
    }

}
