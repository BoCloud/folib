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
package com.folib.services.impl;

import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.components.StorageClientComponent;
import com.folib.components.NpmComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.client.ResponseResult;
import com.folib.enums.NpmPacketSuffix;
import com.folib.enums.NpmRepositoryTypeEnum;
import com.folib.enums.NpmSubLayout;
import com.folib.enums.ResponseDataTypeEnum;
import com.folib.npm.metadata.Dist;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.npm.metadata.Versions;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.NpmProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.folib.utils.NpmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.net.URI;
import java.nio.file.Files;
import java.util.Map;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class NpmProxyProvider implements NpmProvider {

    @Inject
    private NpmProviderRegistry npmProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private NpmComponent npmComponent;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @PostConstruct
    @Override
    public void register() {
        npmProviderRegistry.addProvider(NpmRepositoryTypeEnum.NPM_PROXY.getType(), this);
        log.info("Registered npm provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), NpmRepositoryTypeEnum.NPM_PROXY.getType());
    }

    @Override
    public PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageFeed packageFeed = packageFeed(repository, packageName, targetUrl);
        if (Objects.nonNull(packageFeed)) {
            log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
            PackageVersion packageVersion = packageFeed.getVersions().getAdditionalProperties().get(version);
            if (Objects.nonNull(packageVersion)) {
                log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                return packageVersion;
            }
        }
        return null;
    }

    @Override
    public PackageVersion getLocalPackageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageFeed packageFeed = getLocalPackageFeed(repository, packageName, targetUrl);
        if (Objects.nonNull(packageFeed)) {
            log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
            PackageVersion packageVersion = packageFeed.getVersions().getAdditionalProperties().get(version);
            if (Objects.nonNull(packageVersion)) {
                log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                return packageVersion;
            }
        }
        return null;
    }

    @Override
    public PackageFeed packageFeed(Repository repository, String packageName, String targetUrl) {
        String packageFeedFilePath = NpmUtils.getPackageMetadataPath(packageName);
        RepositoryPath packageFeedJsonRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
        long startTime = System.currentTimeMillis();
        try {
            String data = null;
            PackageFeed packageFeed = null;
            if (Objects.isNull(packageFeedJsonRepositoryPath) || !Files.exists(packageFeedJsonRepositoryPath) || RepositoryFiles.hasRefreshContent(packageFeedJsonRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(packageFeedJsonRepositoryPath)) {
                    log.info("Npm indexJsonRepositoryPath [{}] [{}] [{}] refresh content", packageFeedJsonRepositoryPath.getStorageId(), packageFeedJsonRepositoryPath.getRepositoryId(), packageName);
                }
                data = commonUrlJSONData(repository, targetUrl);
                log.info("请求 [{}] 耗时[{}] ms", targetUrl, System.currentTimeMillis() - startTime);
                if (StringUtils.isBlank(data)) {
                    return npmComponent.readPackageFeed(packageFeedJsonRepositoryPath);
                }
                try {
                    startTime = System.currentTimeMillis();
                    String storageId = repository.getStorage().getId();
                    String repositoryId = repository.getId();
                    String repositoryBaseUrl = getRepositoryBaseUrl(repository);
                    packageFeed = npmComponent.convertToPackageFeed(packageFeedJsonRepositoryPath, data);
                    if (Objects.isNull(packageFeed)) {
                        return null;
                    }
                    log.info("转换 package 耗时[{}] ms", System.currentTimeMillis() - startTime);
                    Versions versions = packageFeed.getVersions();
                    if (Objects.isNull(versions)) {
                        log.warn("Npm indexJsonRepositoryPath [{}] [{}] [{}] versions is null", storageId, repositoryId, packageName);
                        return null;
                    }
                    final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
                    startTime = System.currentTimeMillis();
                    for (Map.Entry<String, PackageVersion> versionEntry : versions.getAdditionalProperties().entrySet()) {
                        handleVersion(storageId, repositoryId, repositoryBaseUrl, versionEntry, packageSuffix);
                    }
                    log.info("处理 tarball 耗时[{}] ms", System.currentTimeMillis() - startTime);
                    startTime = System.currentTimeMillis();
                    npmComponent.writePackageFeed(packageFeedJsonRepositoryPath, packageFeed);
                    log.info("写入 package 耗时[{}] ms", System.currentTimeMillis() - startTime);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
            return npmComponent.readPackageFeed(packageFeedJsonRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public PackageFeed getLocalPackageFeed(Repository repository, String packageName, String targetUrl) {
        String packageFeedFilePath = NpmUtils.getPackageMetadataPath(packageName);
        RepositoryPath packageFeedJsonRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
        try {
            if (Objects.nonNull(packageFeedJsonRepositoryPath) && Files.exists(packageFeedJsonRepositoryPath) && !RepositoryFiles.hasRefreshContent(packageFeedJsonRepositoryPath)) {
                return npmComponent.readPackageFeed(packageFeedJsonRepositoryPath);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String binary(Repository repository, String packageName, String targetUrl) {
        String binaryFilePath = NpmUtils.getBinaryMetadataPath(packageName);
        RepositoryPath binaryJsonRepositoryPath = repositoryPathResolver.resolve(repository, binaryFilePath);
        long startTime = System.currentTimeMillis();
        try {
            String data = null;
            if (Objects.isNull(binaryJsonRepositoryPath) || !Files.exists(binaryJsonRepositoryPath) || RepositoryFiles.hasRefreshContent(binaryJsonRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(binaryJsonRepositoryPath)) {
                    log.info("Npm indexJsonRepositoryPath [{}] [{}] [{}] refresh content", binaryJsonRepositoryPath.getStorageId(), binaryJsonRepositoryPath.getRepositoryId(), packageName);
                }
                data = commonUrlJSONData(repository, targetUrl);
                log.info("请求 [{}] 耗时[{}] ms", targetUrl, System.currentTimeMillis() - startTime);
                if (StringUtils.isBlank(data)) {
                    return npmComponent.readBinary(binaryJsonRepositoryPath);
                }
                try {
                    String repositoryBaseUrl = getRepositoryBaseUrl(repository);
                    data = npmComponent.handleBinary(repositoryBaseUrl, data);
                    if (StringUtils.isBlank(data)) {
                        return null;
                    }
                    startTime = System.currentTimeMillis();
                    npmComponent.writeBinary(binaryJsonRepositoryPath, packageName, data);
                    log.info("写入 binary 耗时[{}] ms", System.currentTimeMillis() - startTime);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
            return npmComponent.readBinary(binaryJsonRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String getLocalBinary(Repository repository, String packageName, String targetUrl) {
        String binaryFilePath = NpmUtils.getBinaryMetadataPath(packageName);
        RepositoryPath binaryJsonRepositoryPath = repositoryPathResolver.resolve(repository, binaryFilePath);
        try {
            if (Objects.nonNull(binaryJsonRepositoryPath) && Files.exists(binaryJsonRepositoryPath) && !RepositoryFiles.hasRefreshContent(binaryJsonRepositoryPath)) {
                return npmComponent.readBinary(binaryJsonRepositoryPath);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private void handleVersion(String storageId, String repositoryId, String repositoryBaseUrl, Map.Entry<String, PackageVersion> versionEntry, String packageSuffix) {
        NpmCoordinates npmArtifactCoordinates = null;
        try {
            Dist dist = versionEntry.getValue().getDist();
            if (Objects.nonNull(dist) && StringUtils.isNotBlank(dist.getTarball())) {
                npmArtifactCoordinates = NpmCoordinates.of(versionEntry.getValue().getName(), versionEntry.getValue().getVersion(), packageSuffix);
                URI uri = npmArtifactCoordinates.convertToResource(npmArtifactCoordinates);
                dist.setTarball(repositoryBaseUrl + uri.toString());
            }
        } catch (Exception ex) {
            log.warn("Npm storageId [{}] repositoryId [{}] packageName [{}] version [{}] parse error [{}]", storageId, repositoryId, npmArtifactCoordinates.getName(), npmArtifactCoordinates.getVersion(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private String commonUrlJSONData(Repository repository, String url) {
        String data = null;
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
        String prefixUrl = remoteRepository.getUrl();
        String suffixUrl = url;
        if (!suffixUrl.startsWith(GlobalConstants.SEPARATOR)) {
            suffixUrl = GlobalConstants.SEPARATOR + suffixUrl;
        }
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.JSON.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            data = responseResult.getData();
        }
        return data;
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}
