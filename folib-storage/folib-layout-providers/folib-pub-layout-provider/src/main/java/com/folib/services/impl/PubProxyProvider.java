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

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.folib.artifact.coordinates.PubCoordinates;
import com.folib.components.StorageClientComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.PubPackageMetadata;
import com.folib.domain.PubPackageVersionMetadata;
import com.folib.domain.client.ResponseResult;
import com.folib.enums.PubRepositoryTypeEnum;
import com.folib.enums.ResponseDataTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.PubProvider;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.folib.utils.PubUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.net.URI;
import java.nio.file.Files;
import java.util.Objects;
import java.util.Optional;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PubProxyProvider implements PubProvider {

    @Inject
    private PubProviderRegistry pubProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    @PostConstruct
    @Override
    public void register() {
        pubProviderRegistry.addProvider(PubRepositoryTypeEnum.PUB_PROXY.getType(), this);
        log.info("Registered pub provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PubRepositoryTypeEnum.PUB_PROXY.getType());
    }

    @Override
    public PubPackageVersionMetadata inspectVersion(Repository repository, String packageName, String version, String targetUrl) {
        JSONObject pubPackageMetadataJson = packages(repository, packageName, targetUrl);
        if (Objects.nonNull(pubPackageMetadataJson)) {
            PubPackageMetadata pubPackageMetadata = pubPackageMetadataJson.toJavaObject(PubPackageMetadata.class);
            if (Objects.nonNull(pubPackageMetadata)) {
                log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
                Optional<PubPackageVersionMetadata> pubSpecificVersionMetadata = pubPackageMetadata.getVersions().stream().filter(versionMetadata -> versionMetadata.getVersion().equals(version)).findFirst();
                if (pubSpecificVersionMetadata.isPresent()) {
                    log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                    return pubSpecificVersionMetadata.get();
                }
            }
        }
        return null;
    }

    @Override
    public JSONObject packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PubUtils.getPackageMetadataFilePath(packageName);
        RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            JSONObject packageData = null;
            PubPackageMetadata pubPackageMetadata = null;
            if (Objects.isNull(packageJsonRepositoryPath) || !Files.exists(packageJsonRepositoryPath) || RepositoryFiles.hasRefreshContent(packageJsonRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(packageJsonRepositoryPath)) {
                    log.info("Pub indexJsonRepositoryPath [{}] [{}] [{}] refresh content", packageJsonRepositoryPath.getStorageId(), packageJsonRepositoryPath.getRepositoryId(), packageName);
                }
                packageData = commonUrlJSONData(repository, targetUrl);
                if (Objects.isNull(packageData)) {
                    if (Files.exists(packageJsonRepositoryPath)) {
                        return JSONObject.parseObject(Files.readString(packageJsonRepositoryPath));
                    }
                    return null;
                }
                try {
                    String storageId = repository.getStorage().getId();
                    String repositoryId = repository.getId();
                    String baseUrl = StringUtils.removeEnd(configurationManager.getConfiguration().getBaseUrl(), GlobalConstants.SEPARATOR);
                    String repositoryBaseUrl = baseUrl + String.format("/storages/%s/%s/", storageId, repositoryId);
                    pubPackageMetadata = JSONObject.parseObject(packageData.toJSONString(), PubPackageMetadata.class);
                    for (PubPackageVersionMetadata pubPackageVersionMetadata : pubPackageMetadata.getVersions()) {
                        handleVersion(storageId, repositoryId, repositoryBaseUrl, pubPackageMetadata, PubCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubCoordinates.PUB_EXTENSION), pubPackageVersionMetadata);
                    }
                    PubPackageVersionMetadata pubPackageVersionMetadata = pubPackageMetadata.getLatest();
                    if (Objects.nonNull(pubPackageVersionMetadata)) {
                        handleVersion(storageId, repositoryId, repositoryBaseUrl, pubPackageMetadata, PubCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubCoordinates.PUB_EXTENSION), pubPackageVersionMetadata);
                    }
                    Files.createDirectories(packageJsonRepositoryPath.getParent());
                    Files.writeString(packageJsonRepositoryPath, JSON.toJSONString(pubPackageMetadata, SerializerFeature.PrettyFormat));
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
            return JSONObject.parseObject(Files.readString(packageJsonRepositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public JSONObject getLocalPackages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PubUtils.getPackageMetadataFilePath(packageName);
        RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.nonNull(packageJsonRepositoryPath) && Files.exists(packageJsonRepositoryPath) && !RepositoryFiles.hasRefreshContent(packageJsonRepositoryPath)) {
                return JSONObject.parseObject(Files.readString(packageJsonRepositoryPath));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    private void handleVersion(String storageId, String repositoryId, String repositoryBaseUrl, PubPackageMetadata pubPackageMetadata, PubCoordinates pubArtifactCoordinates, PubPackageVersionMetadata pubPackageVersionMetadata) {
        try {
            if (StringUtils.isNotBlank(pubPackageVersionMetadata.getArchiveUrl())) {
                pubPackageVersionMetadata.setSourceArchiveUrl(pubPackageVersionMetadata.getArchiveUrl());
                pubArtifactCoordinates = PubCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubCoordinates.PUB_EXTENSION);
                URI uri = pubArtifactCoordinates.convertToResource(pubArtifactCoordinates);
                pubPackageVersionMetadata.setArchiveUrl(repositoryBaseUrl + uri.toString());
            }
        } catch (Exception ex) {
            log.warn("Pub storageId [{}] repositoryId [{}] packageName [{}] version [{}] parse error [{}]", storageId, repositoryId, pubArtifactCoordinates.getName(), pubPackageVersionMetadata.getVersion(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private JSONObject commonUrlJSONData(Repository repository, String url) {
        JSONObject data = null;
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
            data = JSONObject.parseObject(responseResult.getData());
        }
        return data;
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}
