package com.veadan.folib.storage.tag;

import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DockerAuthComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.DockerHeaderEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2024/1/19
 **/
@Slf4j
@Component
public class ChecksumTagExpirationStrategy
        implements DockerExpirationStrategy {

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private DockerAuthComponent dockerAuthComponent;

    @Override
    public Decision decide(final RepositoryPath repositoryPath) throws IOException {
        return decideUsingChecksumAlgorithm(repositoryPath);
    }

    private RepositoryPath resolveSiblingChecksum(final RepositoryPath repositoryPath,
                                                  final String checksumAlgorithm) {
        return repositoryPath.resolveSibling(
                repositoryPath.getFileName().toString() + checksumAlgorithm);
    }

    private String readChecksum(final RepositoryPath checksumRepositoryPath)
            throws IOException {
        if (!Files.exists(checksumRepositoryPath)) {
            return null;
        }

        return Files.readAllLines(checksumRepositoryPath).stream().findFirst().orElse(null);
    }

    private Decision decideUsingChecksumAlgorithm(final RepositoryPath repositoryPath) throws IOException {
        String currentChecksum = repositoryPath.getFileName().toString();
        if (currentChecksum == null) {
            log.info("Unable to read local {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                    MessageDigestAlgorithms.SHA_256,
                    repositoryPath.normalize());
            return Decision.UNDECIDED;
        }
        final String newRemoteChecksum = fetchDigest(repositoryPath);
        if (StringUtils.isBlank(newRemoteChecksum)) {
            log.info("Unable to fetch remote {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                    MessageDigestAlgorithms.SHA_256,
                    repositoryPath.normalize());
            return Decision.UNDECIDED;
        }

        if (currentChecksum.equals(newRemoteChecksum)) {
            log.info("Local and remote {} checksums match for {}, no need to refetch metadata",
                    MessageDigestAlgorithms.SHA_256,
                    repositoryPath.normalize());
            return Decision.USABLE;
        } else {
            log.info("Local and remote {} checksums differ for {}, will refetch metadata",
                    MessageDigestAlgorithms.SHA_256,
                    repositoryPath.normalize());
            return Decision.EXPIRED;
        }
    }

    @Override
    public String fetchDigest(RepositoryPath repositoryPath) throws IOException {
        DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
        if (Objects.isNull(dockerArtifactCoordinates)) {
            log.warn("RepositoryPath [{}] resolve dockerArtifactCoordinates is null", repositoryPath);
            return "";
        }
        String tag = dockerArtifactCoordinates.getTAG(), digest = "";
        RemoteRepository remoteRepository = repositoryPath.getRepository().getRemoteRepository();
        String remoteUrl = StringUtils.removeEnd(remoteRepository.getUrl(), GlobalConstants.SEPARATOR);
        if (remoteUrl.endsWith(GlobalConstants.DOCKER_V2)) {
            String imagePath = dockerArtifactCoordinates.getName();
            if (imagePath.split(GlobalConstants.SEPARATOR).length <= 1) {
                remoteUrl = remoteUrl.concat(GlobalConstants.SEPARATOR).concat(GlobalConstants.DOCKER_DEFAULT_REPO);
            }
        }
        String targetUrl = String.format("%s/%s/manifests/%s", remoteUrl, StringUtils.removeEnd(dockerArtifactCoordinates.getName(), GlobalConstants.SEPARATOR), tag);
        Response response = null;
        try {
            Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient(repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
            //连接建立超时时间
            client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
            //读取内容超时时间
            client.property(ClientProperties.READ_TIMEOUT, 30000);
            WebTarget target = client.target(targetUrl);
            final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(remoteRepository.getUsername()) && StringUtils.isNotBlank(remoteRepository.getPassword())) ? HttpAuthenticationFeature.basic(remoteRepository.getUsername(), remoteRepository.getPassword()) : null;
            if (Objects.nonNull(authenticationFeature)) {
                target.register(authenticationFeature);
            }
            Invocation.Builder builder = target.request();
            MultivaluedMap<String, Object> headers = DockerHeaderEnum.acceptHeaders();
            dockerAuthComponent.handleAuthToken(remoteRepository, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), dockerArtifactCoordinates.getName(), headers);
            builder = builder.headers(headers);
            response = builder.head();
            String responseBody = response.readEntity(String.class);
            if (HttpStatus.SC_OK != response.getStatus()) {
                throw new RuntimeException(String.format("Url response error [%s] [%s]", response.getStatus(), responseBody));
            }
            digest = response.getHeaderString(DockerHeaderEnum.DOCKER_CONTENT_DIGEST.key());
            log.info("Resolve repositoryPath [{}] remote digest [{}]", repositoryPath, digest);
        } catch (Exception ex) {
            log.error("Resolve repositoryPath [{}] digest error [{}]", repositoryPath, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        return digest;
    }


}
