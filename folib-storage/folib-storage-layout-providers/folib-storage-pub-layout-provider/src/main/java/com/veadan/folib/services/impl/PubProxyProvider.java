package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.component.StorageClientComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.constants.PubConstants;
import com.veadan.folib.domain.PubPackageMetadata;
import com.veadan.folib.domain.PubPackageVersionMetadata;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.PubRepositoryTypeEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.PubProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.PubUtils;
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
 * @author leipenghui
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
                    return null;
                }
                try {
                    String storageId = repository.getStorage().getId();
                    String repositoryId = repository.getId();
                    String baseUrl = StringUtils.removeEnd(configurationManager.getConfiguration().getBaseUrl(), GlobalConstants.SEPARATOR);
                    String repositoryBaseUrl = baseUrl + String.format("/storages/%s/%s/", storageId, repositoryId);
                    pubPackageMetadata = JSONObject.parseObject(packageData.toJSONString(), PubPackageMetadata.class);
                    for (PubPackageVersionMetadata pubPackageVersionMetadata : pubPackageMetadata.getVersions()) {
                        handleVersion(storageId, repositoryId, repositoryBaseUrl, pubPackageMetadata, PubArtifactCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubArtifactCoordinates.PUB_EXTENSION), pubPackageVersionMetadata);
                    }
                    PubPackageVersionMetadata pubPackageVersionMetadata = pubPackageMetadata.getLatest();
                    if (Objects.nonNull(pubPackageVersionMetadata)) {
                        handleVersion(storageId, repositoryId, repositoryBaseUrl, pubPackageMetadata, PubArtifactCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubArtifactCoordinates.PUB_EXTENSION), pubPackageVersionMetadata);
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

    private void handleVersion(String storageId, String repositoryId, String repositoryBaseUrl, PubPackageMetadata pubPackageMetadata, PubArtifactCoordinates pubArtifactCoordinates, PubPackageVersionMetadata pubPackageVersionMetadata) {
        try {
            if (StringUtils.isNotBlank(pubPackageVersionMetadata.getArchiveUrl())) {
                pubPackageVersionMetadata.setSourceArchiveUrl(pubPackageVersionMetadata.getArchiveUrl());
                pubArtifactCoordinates = PubArtifactCoordinates.of(pubPackageMetadata.getName(), pubPackageVersionMetadata.getVersion(), PubArtifactCoordinates.PUB_EXTENSION);
                URI uri = pubArtifactCoordinates.convertToResource(pubArtifactCoordinates);
                pubPackageVersionMetadata.setArchiveUrl(repositoryBaseUrl + uri.toString());
            }
        } catch (Exception ex) {
            log.warn("Pub storageId [{}] repositoryId [{}] packageName [{}] version [{}] parse error [{}]", storageId, repositoryId, pubArtifactCoordinates.getName(), pubPackageVersionMetadata.getVersion(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private JSONObject commonUrlJSONData(Repository repository, String url) {
        JSONObject data = null;
        String prefixUrl = repository.getRemoteRepository().getUrl();
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
