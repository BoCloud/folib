package com.veadan.folib.services.impl;

import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.components.StorageClientComponent;
import com.veadan.folib.components.NpmComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.NpmPacketSuffix;
import com.veadan.folib.enums.NpmRepositoryTypeEnum;
import com.veadan.folib.enums.NpmSubLayout;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.npm.metadata.Dist;
import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.npm.metadata.Versions;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.NpmProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import com.veadan.folib.utils.NpmUtils;
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
        NpmArtifactCoordinates npmArtifactCoordinates = null;
        try {
            Dist dist = versionEntry.getValue().getDist();
            if (Objects.nonNull(dist) && StringUtils.isNotBlank(dist.getTarball())) {
                npmArtifactCoordinates = NpmArtifactCoordinates.of(versionEntry.getValue().getName(), versionEntry.getValue().getVersion(), packageSuffix);
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
