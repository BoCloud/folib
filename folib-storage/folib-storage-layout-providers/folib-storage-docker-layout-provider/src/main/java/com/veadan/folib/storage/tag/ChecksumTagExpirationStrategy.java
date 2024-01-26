package com.veadan.folib.storage.tag;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.DockerClientComponent;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.AuthInfo;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.DockerHeaderEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.util.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
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
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    private DockerClientComponent dockerClientComponent;

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
        String targetUrl = String.format("%s/%s/manifests/%s", StringUtils.removeEnd(remoteRepository.getUrl(), GlobalConstants.SEPARATOR), StringUtils.removeEnd(dockerArtifactCoordinates.getName(), GlobalConstants.SEPARATOR), tag);
        Response response = null;
        try {
            Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient(repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
            WebTarget target = client.target(targetUrl);
            final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(remoteRepository.getUsername()) && StringUtils.isNotBlank(remoteRepository.getPassword())) ? HttpAuthenticationFeature.basic(remoteRepository.getUsername(), remoteRepository.getPassword()) : null;
            if (Objects.nonNull(authenticationFeature)) {
                target.register(authenticationFeature);
            }
            Invocation.Builder builder = target.request();
            MultivaluedMap<String, Object> headers = DockerHeaderEnum.acceptHeaders();
            handleToken(remoteRepository, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), dockerArtifactCoordinates.getName(), headers);
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

    private void handleToken(RemoteRepository remoteRepository, String storageId, String repositoryId, String imagePath, MultivaluedMap<String, Object> requestHeaders) {
        if (Objects.isNull(remoteRepository)) {
            return;
        }
        String end = "/v2/", remoteUrl = remoteRepository.getUrl();
        if (!remoteUrl.endsWith(GlobalConstants.SEPARATOR)) {
            remoteUrl = remoteUrl.concat(GlobalConstants.SEPARATOR);
        }
        int index = remoteUrl.indexOf(end) + end.length();
        String targetUrl = remoteUrl.substring(0, index);
        ResponseResult responseResult = dockerClientComponent.doGet(storageId, repositoryId, targetUrl);
        if (Objects.isNull(responseResult)) {
            return;
        }
        MultivaluedMap<String, String> headers = responseResult.getHeaders();
        if (MapUtils.isEmpty(headers)) {
            return;
        }
        String authenticate = headers.getFirst("WWW-Authenticate");
        if (StringUtils.isBlank(authenticate)) {
            return;
        }
        String pattern = "realm=\"(.*?)\"";
        String authUrl = resolveAuthParam(authenticate, pattern);
        if (StringUtils.isBlank(authUrl)) {
            return;
        }
        pattern = "service=\"(.*?)\"";
        String service = resolveAuthParam(authenticate, pattern);
        if (StringUtils.isBlank(service)) {
            return;
        }
        String scope = "repository:%s%s:pull";
        String scopeRepository = "library/";
        if (imagePath.split(GlobalConstants.SEPARATOR).length > 1) {
            scopeRepository = "";
        }
        scope = String.format(scope, scopeRepository, StringUtils.removeEnd(imagePath, GlobalConstants.SEPARATOR));
        Map<String, String> params = Maps.newHashMap();
        params.put("scope", scope);
        params.put("service", service);
        authUrl = authUrl + CommonUtils.createLinkStringByGet(params);
        responseResult = dockerClientComponent.doGet(storageId, repositoryId, authUrl);
        if (StringUtils.isBlank(responseResult.getData()) || !ResponseDataTypeEnum.JSON.getType().equals(responseResult.getDataType().getType())) {
            return;
        }
        AuthInfo authInfo = JSONObject.parseObject(responseResult.getData(), AuthInfo.class);
        if (StringUtils.isBlank(authInfo.getToken()) || Objects.isNull(authInfo.getExpiresIn())) {
            return;
        }
        requestHeaders.put("Authorization", Collections.singletonList(String.format("%s %s", "Bearer", authInfo.getToken())));
    }

    private String resolveAuthParam(String authenticate, String pattern) {
        Pattern r = Pattern.compile(pattern);
        Matcher m = r.matcher(authenticate);
        if (m.find()) {
            return m.group(1);
        } else {
            return "";
        }
    }

}
