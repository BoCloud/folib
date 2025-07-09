/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.tag;

import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.DockerAuthComponent;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.constant.GlobalConstants;
import com.folib.enums.DockerHeaderEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.storage.repository.remote.RemoteRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.client.ClientProperties;
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
 * @author veadan
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
        DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
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
            String username = remoteRepository.getUsername(), password = remoteRepository.getPassword();
            final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
            if (Objects.nonNull(customAuthenticationFeature)) {
                target.register(customAuthenticationFeature);
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
