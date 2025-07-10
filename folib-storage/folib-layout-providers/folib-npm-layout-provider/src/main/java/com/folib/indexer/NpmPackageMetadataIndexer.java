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
package com.folib.indexer;

import com.github.zafarkhaja.semver.Version;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.components.DistributedLockComponent;
import com.folib.components.NpmComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.constants.NpmConstants;
import com.folib.data.criteria.Paginator;
import com.folib.enums.NpmIndexTypeEnum;
import com.folib.enums.NpmSubLayout;
import com.folib.npm.metadata.*;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.NpmPackageSupplier;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.services.NpmService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.utils.NpmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.javatuples.Pair;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @date 2024/6/15
 **/
@Slf4j
@Component
public class NpmPackageMetadataIndexer {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private NpmService npmService;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Inject
    private NpmPackageSupplier npmPackageSupplier;

    @Inject
    @Lazy
    private NpmComponent npmComponent;

    public void indexAsSystem(RepositoryPath repositoryPath, NpmIndexTypeEnum npmIndexTypeEnum) throws Exception {
        if (Objects.isNull(repositoryPath) || Files.isHidden(repositoryPath)) {
            return;
        }
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType())) {
            return;
        }
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        NpmCoordinates npmArtifactCoordinates;
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        if (artifactPath.startsWith(NpmConstants.PACKAGE_JSON_PATH) || NpmConstants.METADATA_EXTENSION_LIST.stream().anyMatch(artifactPath::endsWith)) {
            return;
        }
        if (NpmConstants.EXTENSION_LIST.stream().noneMatch(artifactPath::endsWith)) {
            npmArtifactCoordinates = NpmCoordinates.resolveName(repositoryPath, artifactPath);
            String extension = NpmSubLayout.OHPM.getValue().equals(repositoryPath.getRepository().getSubLayout()) ? NpmConstants.EXTENSION_HAR : NpmConstants.EXTENSION_TGZ;
            npmArtifactCoordinates.setExtension(extension);
        } else {
            npmArtifactCoordinates = NpmCoordinates.parse(artifactPath);
        }
        String key = String.format("NpmIndex_%s_%s_%s", storageId, repositoryId, npmArtifactCoordinates.getName());
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                PackageFeed packageFeed;
                String packageName = NpmCoordinates.calculatePackageId(npmArtifactCoordinates.getScope(), npmArtifactCoordinates.getName());
                if (shouldReindexPackage(npmIndexTypeEnum)) {
                    log.info("Found Reindex for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    packageFeed = reindexPackage(repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName);
                } else {
                    String packageMetadataFilePath = NpmUtils.getPackageMetadataPath(packageName);
                    RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
                    packageFeed = npmComponent.readPackageFeed(packageJsonRepositoryPath);
                    log.debug("Started indexing Npm package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    indexType(repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName, npmIndexTypeEnum, packageFeed);
                    log.debug("Finished indexing Npm package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                }
                finalizePackageIndexing(repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName, npmIndexTypeEnum, packageFeed);
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    private RepositorySearchRequest createSearchPredicate(String packageScope,
                                                          String packageName, String subLayout) {
        List<String> coordinateValues = NpmSubLayout.OHPM.getValue().equals(subLayout) ? Lists.newArrayList("har") : Lists.newArrayList("tgz");
        RepositorySearchRequest rootPredicate = new RepositorySearchRequest(
                NpmCoordinates.calculatePackageId(packageScope, packageName), Lists.newArrayList(coordinateValues));

        return rootPredicate;
    }

    private String generateRevisionHashcode(PackageFeed packageFeed) {
        String versionsShaSum = packageFeed.getVersions().getAdditionalProperties()
                .values()
                .stream()
                .map(x -> x.getDist().getShasum())
                .collect(Collectors.joining());
        return packageFeed.getVersions().getAdditionalProperties().size() + "-" +
                DigestUtils.sha1Hex(versionsShaSum).substring(0, 16);
    }

    private PackageFeed reindexPackage(RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.info("Started reindexing for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
        PackageFeed packageFeed = new PackageFeed();
        Versions versions = new Versions();
        packageFeed.setVersions(versions);
        String packageId = NpmCoordinates.calculatePackageId(npmArtifactCoordinates.getScope(), npmArtifactCoordinates.getName());
        packageFeed.setName(packageId);
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repositoryPath.getRepository().getType());
        RepositorySearchRequest predicate = createSearchPredicate(npmArtifactCoordinates.getScope(), npmArtifactCoordinates.getName(), repositoryPath.getRepository().getSubLayout());
        predicate.setNotPublishEvent(true);
        Paginator paginator = new Paginator();
        paginator.setProperty("version");
        paginator.setUseLimit(false);
        paginator.setLimit(9999);
        List<Path> packageResultList = repositoryProvider.search(storageId, repositoryId, predicate,
                paginator);
        log.info("Found '{}' versions for package '{}' in storage '{}' repository '{}'", packageResultList.size(), packageName, storageId, repositoryId);
        if (CollectionUtils.isNotEmpty(packageResultList)) {
            packageFeed.setAdditionalProperty("_id", packageId);
            Time npmTime = new Time();
            packageFeed.setTime(npmTime);
            DistTags distTags = new DistTags();
            packageFeed.setDistTags(distTags);
            packageResultList.stream().map(npmPackageSupplier).forEach(p -> {
                PackageVersion npmPackage = p.getNpmPackage();
                versions.setAdditionalProperty(npmPackage.getVersion(), npmPackage);
                npmTime.setAdditionalProperty(npmPackage.getVersion(), p.getReleaseDate());
                Date created = npmTime.getCreated();
                npmTime.setCreated(created == null || created.before(p.getReleaseDate()) ? p.getReleaseDate() : created);
                Date modified = npmTime.getModified();
                npmTime.setModified(modified == null || modified.before(p.getReleaseDate()) ? p.getReleaseDate()
                        : modified);
            });
            packageFeed.setAdditionalProperty("_rev", generateRevisionHashcode(packageFeed));
        } else {
            String packageMetadataFilePath = NpmUtils.getPackageMetadataPath(packageFeed.getName());
            RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
            log.info("Metadata for package contains no versions, deleting file in '{}'", packageJsonRepositoryPath);
            Files.deleteIfExists(packageJsonRepositoryPath);
            return null;
        }
        return packageFeed;
    }

    private boolean shouldReindexPackage(NpmIndexTypeEnum npmIndexTypeEnum) {
        return NpmIndexTypeEnum.REINDEX.getType().equals(npmIndexTypeEnum.getType());
    }

    private void finalizePackageIndexing(RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName, NpmIndexTypeEnum npmIndexTypeEnum, PackageFeed packageFeed) throws Exception {
        if (Objects.isNull(packageFeed) || Objects.isNull(packageFeed.getVersions())) {
            return;
        }
        Map<String, PackageVersion> packageVersionMap = packageFeed.getVersions().getAdditionalProperties();
        if (MapUtils.isNotEmpty(packageVersionMap)) {
            log.info("Finished updating package '{}' metadata, package contains now {} versions", packageFeed
                    .getName(), packageVersionMap.size());
            List<String> versionList = Lists.newArrayList(packageVersionMap.keySet());
            sort(versionList);
            if (CollectionUtils.isNotEmpty(versionList)) {
                packageFeed.getDistTags().setLatest(versionList.get(0));
            }
        }
        writeOrDeletePackageMetadataFile(repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName, npmIndexTypeEnum, packageFeed);
    }

    private void indexType(RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName, NpmIndexTypeEnum npmIndexTypeEnum, PackageFeed packageFeed) {
        try {
            switch (npmIndexTypeEnum.getType()) {
                case "add":
                    handleAddNpmPackage(packageFeed, repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName);
                    break;
                case "delete":
                    handleDeleteNpmPackage(packageFeed, repositoryPath, npmArtifactCoordinates, storageId, repositoryId, packageName);
                    break;
                default:
                    ;
            }
        } catch (Exception e) {
            log.warn("Error occurred during index processing error [{}]", ExceptionUtils.getStackTrace(e));
        }
    }

    private void writeOrDeletePackageMetadataFile(RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName, NpmIndexTypeEnum npmIndexTypeEnum, PackageFeed packageFeed) throws Exception {
        String packageMetadataFilePath = NpmUtils.getPackageMetadataPath(packageFeed.getName());
        RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
        if (MapUtils.isNotEmpty(packageFeed.getVersions().getAdditionalProperties())) {
            log.info("Writing metadata for storageId [{}] repositoryId [{}] packageName [{}] , package contains now {} versions", storageId, repositoryId, packageFeed.getName(), packageFeed.getAdditionalProperties().size());
            log.debug("Writing metadata to: '{}'", packageFeed);
            npmComponent.writePackageFeed(packageJsonRepositoryPath, packageFeed);
        } else {
            log.info("Metadata for package contains no versions, deleting file in '{}'", packageJsonRepositoryPath);
            Files.deleteIfExists(packageJsonRepositoryPath);
        }
    }

    private void handleDeleteNpmPackage(PackageFeed packageFeed, RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.debug("Handling Delete for package: '{}', version: '{}'", packageName, npmArtifactCoordinates.getVersion());
        removeVersionFromPackageMetadata(packageFeed, npmArtifactCoordinates.getVersion());
    }

    private void handleAddNpmPackage(PackageFeed packageFeed, RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.debug("Handling Add for package: '{}', version: '{}'", packageName, npmArtifactCoordinates.getVersion());
        addNpmPackage(repositoryPath, npmArtifactCoordinates, packageName, packageFeed);
    }

    private void addNpmPackage(RepositoryPath repositoryPath, NpmCoordinates npmArtifactCoordinates, String packageName, PackageFeed packageFeed) throws Exception {
        try (InputStream inputStream = Files.newInputStream(repositoryPath);) {
            Pair<PackageVersion, Path> packageVersionPathPair = npmComponent.extractPackage(packageName, inputStream, repositoryPath.getRepository().getSubLayout());
            PackageVersion packageVersion = packageVersionPathPair.getValue0();
            String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
            URI uri = npmArtifactCoordinates.convertToResource(npmArtifactCoordinates);
            Dist dist = packageVersion.getDist();
            if (Objects.nonNull(dist)) {
                dist.setTarball(repositoryBaseUrl + uri.toString());
            }
            addVersionToPackageMetaData(packageVersion, packageFeed);
        }
    }

    private void removeVersionFromPackageMetadata(PackageFeed packageFeed, String packageVersion) {
        log.debug("Remove version {} from package {}", packageVersion, packageFeed.getName());
        Map<String, PackageVersion> packageVersionMap = packageFeed.getVersions().getAdditionalProperties();
        packageVersionMap.remove(packageVersion);
        Map<String, Date> timeMap = packageFeed.getTime().getAdditionalProperties();
        timeMap.remove(packageVersion);
    }

    private void addVersionToPackageMetaData(PackageVersion version, PackageFeed packageFeed) {
        log.info("Adding version '{}' to package metadata versions", version.getVersion());
        Map<String, PackageVersion> packageVersionMap = packageFeed.getVersions().getAdditionalProperties();
        Map<String, PackageVersion> versions = MapUtils.isEmpty(packageVersionMap) ? Maps.newLinkedHashMap() : packageVersionMap;
        versions.put(version.getVersion(), version);
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

    private void sort(List<String> packageVersionList) {
        filterInvalidVersions(packageVersionList);
        packageVersionList.sort((o1, o2) -> Version.BUILD_AWARE_ORDER.compare(Version.valueOf(o2), Version.valueOf(o1)));
    }

    private void filterInvalidVersions(List<String> packageVersionList) {
        for (String version : packageVersionList) {
            try {
                Version.valueOf(version);
            } catch (Exception e) {
                packageVersionList.remove(version);
                log.warn("version {} is not a valid semver 2 version, version will not be added", version, e);
            }
        }
    }

}

