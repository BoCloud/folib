package com.folib.providers.io;

import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.enums.DockerHeaderEnum;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.repository.ProxyRepositoryProvider;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.tag.ChecksumTagExpirationStrategy;
import com.folib.storage.tag.DockerExpirationStrategy;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import static com.folib.storage.tag.DockerExpirationStrategy.Decision.USABLE;

/**
 * @author veadan
 * @date 2024/1/19
 **/
@Slf4j
@Component
public class DockerTagExpiredRepositoryPathHandler
        implements DockerExpiredRepositoryPathHandler {

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private ChecksumTagExpirationStrategy checksumTagExpirationStrategy;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private ProxyRepositoryProvider proxyRepositoryProvider;

    @Override
    public boolean supports(final RepositoryPath repositoryPath)
            throws IOException {
        if (repositoryPath == null) {
            return false;
        }

        if (!DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()) || RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) || !DockerCoordinates.isDockerTag(repositoryPath)) {
            return false;
        }

        Repository repository = repositoryPath.getRepository();
        return ((RepositoryData) repository).getRemoteRepository() != null;
    }

    @Override
    public void handleExpiration(final RepositoryPath repositoryPath)
            throws IOException {
        Repository repository = repositoryPath.getRepository();
        if (RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            handlerRepositoryPath(repositoryPath);
        }
    }

    private void handlerRepositoryPath(final RepositoryPath repositoryPath) throws IOException {
        DockerExpirationStrategy dockerExpirationStrategy = getTagStrategy();
        DockerExpirationStrategy.Decision fetchTag = dockerExpirationStrategy.decide(repositoryPath);

        if (fetchTag == USABLE) {
            return;
        }
        DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
        String digest = dockerExpirationStrategy.fetchDigest(repositoryPath);
        if (StringUtils.isBlank(digest)) {
            return;
        }
        String targetUrl = String.format("%s/manifests/%s", StringUtils.removeEnd(dockerArtifactCoordinates.getName(), "/"), digest);
        RepositoryPath refreshRepositoryPath = repositoryPath.resolveSibling(digest);
        refreshRepositoryPath.setHeaders(DockerHeaderEnum.acceptHeaders());
        refreshRepositoryPath.setTargetUrl(targetUrl);
        proxyRepositoryProvider.resolvePathExclusive(refreshRepositoryPath);
        RepositoryPath manifestRepositoryPath = repositoryPath.getRoot().resolve(DockerLayoutProvider.MANIFEST).resolve(digest);
        if (!Files.exists(manifestRepositoryPath)) {
            artifactManagementService.store(manifestRepositoryPath, refreshRepositoryPath);
        }
        RepositoryFiles.delete(repositoryPath, true);
    }

    private DockerExpirationStrategy getTagStrategy() {
        return checksumTagExpirationStrategy;
    }

}
