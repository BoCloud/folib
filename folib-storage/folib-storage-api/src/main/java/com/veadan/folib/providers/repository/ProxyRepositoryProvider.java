package com.veadan.folib.providers.repository;

import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.io.RepositoryStreamReadContext;
import com.veadan.folib.io.RepositoryStreamWriteContext;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

/**
 * @author Veadan
 * @author veadan
 */
@Component
public class ProxyRepositoryProvider
        extends AbstractRepositoryProvider {

    private static final Logger logger = LoggerFactory.getLogger(ProxyRepositoryProvider.class);

    private static final String ALIAS = "proxy";

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private HostedRepositoryProvider hostedRepositoryProvider;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath path)
            throws IOException {
        return hostedRepositoryProvider.getInputStreamInternal(path);
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath targetPath = hostedRepositoryProvider.fetchPath(repositoryPath);
        if (targetPath == null) {
            targetPath = resolvePathExclusive(repositoryPath);
        } else if (RepositoryFiles.hasRefreshContent(targetPath)) {
            targetPath = resolvePathExclusive(repositoryPath);
            if (Objects.isNull(targetPath)) {
                targetPath = hostedRepositoryProvider.fetchPath(repositoryPath);
            }
        }
        if (Objects.nonNull(targetPath) && RepositoryFiles.hasExpired(targetPath) && !Files.isDirectory(targetPath)) {
            if (StringUtils.isNotBlank(repositoryPath.getArtifactPath())) {
                eventPublisher.publishEvent(new ProxyRepositoryPathExpiredEvent(repositoryPathResolver.resolve(targetPath.getRepository(), repositoryPath.getArtifactPath())));
            } else {
                eventPublisher.publishEvent(new ProxyRepositoryPathExpiredEvent(targetPath));
            }
        }
        return targetPath;
    }

    public RepositoryPath resolvePathExclusive(RepositoryPath repositoryPath)
            throws IOException {
        try {
            if (Boolean.TRUE.equals(repositoryPath.getDisableRemote())) {
                return null;
            }
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryPath.getRepository().getLayout());
            layoutProvider.targetUrl(repositoryPath);
            return proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
        } catch (IOException e) {
            logger.error("Failed to resolve Path for proxied artifact [{}]",
                    repositoryPath, e);

            throw e;
        }
    }

    @Override
    protected OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator) {
        if (Objects.isNull(predicate.getNotPublishEvent()) || Boolean.FALSE.equals(predicate.getNotPublishEvent())) {
            RemoteRepositorySearchEvent event = new RemoteRepositorySearchEvent(storageId,
                    repositoryId,
                    predicate,
                    paginator);
            eventPublisher.publishEvent(event);
        }

        return hostedRepositoryProvider.search(storageId, repositoryId, predicate, paginator);
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate) {
        return hostedRepositoryProvider.count(storageId, repositoryId, predicate);
    }

    @Override
    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException {
        Artifact artifactEntry = super.provideArtifact(repositoryPath);
        if (artifactEntry.getNativeId() == null) {
            artifactEntry = new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                    RepositoryFiles.readCoordinates(repositoryPath));
            artifactEntry.setArtifactFileExists(Boolean.FALSE);
        }

        return artifactEntry;
    }

    @Override
    protected boolean shouldStoreArtifact(Artifact artifactEntry) {
        boolean result = super.shouldStoreArtifact(artifactEntry) || !artifactEntry.getArtifactFileExists();
        artifactEntry.setArtifactFileExists(true);

        return result;
    }

    @Override
    public void commit(RepositoryStreamWriteContext ctx)
            throws IOException {
        super.commit(ctx);
    }

    @Override
    public void commitStoreIndex(RepositoryStreamReadContext ctx)
            throws IOException {
        super.commitStoreIndex(ctx);
    }

    @Override
    public void onStoreIndexAfter(RepositoryStreamReadContext ctx)
            throws IOException {
        super.onStoreIndexAfter(ctx);
    }

}
