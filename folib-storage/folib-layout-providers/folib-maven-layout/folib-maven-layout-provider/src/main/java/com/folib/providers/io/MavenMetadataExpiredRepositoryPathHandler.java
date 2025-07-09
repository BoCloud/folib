package com.folib.providers.io;

import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.storage.metadata.maven.ChecksumMetadataExpirationStrategy;
import com.folib.storage.metadata.maven.MetadataExpirationStrategy;
import com.folib.storage.metadata.maven.MetadataExpirationStrategyType;
import com.folib.storage.metadata.maven.RefreshMetadataExpirationStrategy;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.configuration.MavenRepositoryConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Optional;

import static com.folib.storage.metadata.maven.MetadataExpirationStrategy.Decision.USABLE;

/**
 * @author veadan
 */
@Component
public class MavenMetadataExpiredRepositoryPathHandler
        implements MavenExpiredRepositoryPathHandler {

    private static final Logger logger = LoggerFactory.getLogger(MavenMetadataExpiredRepositoryPathHandler.class);

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private ChecksumMetadataExpirationStrategy checksumMetadataExpirationStrategy;

    @Inject
    private RefreshMetadataExpirationStrategy refreshMetadataStrategy;

    @Override
    public boolean supports(final RepositoryPath repositoryPath)
            throws IOException {
        if (repositoryPath == null) {
            return false;
        }

        if (!Maven2LayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()) || !RepositoryFiles.isMetadata(repositoryPath)) {
            return false;
        }

        Repository repository = repositoryPath.getRepository();
        return ((RepositoryData) repository).getRemoteRepository() != null;
    }

    @Override
    public void handleExpiration(final RepositoryPath repositoryPath)
            throws IOException {
        MetadataExpirationStrategy metadataExpirationStrategy = getMetadataStrategy(repositoryPath);
        MetadataExpirationStrategy.Decision refetchMetadata = metadataExpirationStrategy.decide(repositoryPath);

        if (refetchMetadata == USABLE) {
            return;
        }
        proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
    }

    private MetadataExpirationStrategy getMetadataStrategy(final RepositoryPath repositoryPath) {
        MavenRepositoryConfiguration repositoryConfiguration =
                (MavenRepositoryConfiguration) repositoryPath.getRepository().getRepositoryConfiguration();

        String strategy = Optional.ofNullable(repositoryConfiguration)
                .map(MavenRepositoryConfiguration::getMetadataExpirationStrategy)
                .orElse(null);

        if (MetadataExpirationStrategyType.REFRESH == MetadataExpirationStrategyType.ofStrategy(strategy)) {
            return refreshMetadataStrategy;
        }

        return checksumMetadataExpirationStrategy;
    }

}
