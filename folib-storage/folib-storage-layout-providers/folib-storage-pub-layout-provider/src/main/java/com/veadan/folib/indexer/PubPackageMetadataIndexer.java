package com.veadan.folib.indexer;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.constants.PubConstants;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.PubPackageMetadata;
import com.veadan.folib.domain.PubPackageVersionMetadata;
import com.veadan.folib.domain.Pubspec;
import com.veadan.folib.enums.PubIndexTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.services.PubService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.utils.PubUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.boot.autoconfigure.data.RepositoryType;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/6/15
 **/
@Slf4j
@Component
public class PubPackageMetadataIndexer {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private PubService pubService;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    public void indexAsSystem(RepositoryPath repositoryPath, PubIndexTypeEnum pubIndexTypeEnum) throws Exception {
        if (Objects.isNull(repositoryPath) || Files.isHidden(repositoryPath)) {
            return;
        }
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType())) {
            return;
        }
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        PubArtifactCoordinates pubArtifactCoordinates = new PubArtifactCoordinates();
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        if (artifactPath.startsWith(PubConstants.PACKAGE_JSON_PATH)) {
            return;
        }
        if (!repositoryPath.getPath().endsWith(PubArtifactCoordinates.PUB_EXTENSION)) {
            pubArtifactCoordinates.setName(artifactPath);
            pubArtifactCoordinates.setExtension(PubArtifactCoordinates.PUB_EXTENSION);
        } else {
            pubArtifactCoordinates = PubArtifactCoordinates.parse(artifactPath);
        }
        String key = String.format("PubIndex_%s_%s_%s", storageId, repositoryId, pubArtifactCoordinates.getName());
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                PubPackageMetadata packageMetadata;
                String packageName = pubArtifactCoordinates.getName();
                if (shouldReindexPackage(pubIndexTypeEnum)) {
                    log.trace("Found Reindex for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    packageMetadata = reindexPackage(repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName);
                } else {
                    packageMetadata = extractPackageMetadata(repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName);
                    log.debug("Started indexing Pub package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    indexType(repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName, pubIndexTypeEnum, packageMetadata);
                    log.debug("Finished indexing Pub package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                }
                finalizePackageIndexing(repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName, pubIndexTypeEnum, packageMetadata);
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    private PubPackageMetadata reindexPackage(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.info("Started reindexing for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
        PubPackageMetadata packageMetadata = PubPackageMetadata.builder().name(packageName).discontinued(false).build();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repositoryPath.getRepository().getType());
        RepositorySearchRequest predicate = new RepositorySearchRequest(packageName, Collections.singleton("tar.gz"));
        Paginator paginator = new Paginator();
        paginator.setUseLimit(false);
        predicate.setNotPublishEvent(true);
        paginator.setLimit(9999);
        List<Path> packageNameResultList = repositoryProvider.search(storageId, repositoryId, predicate,
                paginator);
        log.info("Found '{}' versions for package '{}' in storage '{}' repository '{}'", packageNameResultList.size(), packageName, storageId, repositoryId);
        if (CollectionUtils.isNotEmpty(packageNameResultList)) {
            packageNameResultList.forEach(path -> {
                try {
                    if (Files.exists(path)) {
                        RepositoryPath itemRepositoryPath = (RepositoryPath) path;
                        PubArtifactCoordinates itemPubArtifactCoordinates = PubArtifactCoordinates.parse(RepositoryFiles.relativizePath(itemRepositoryPath));
                        addPubPackage(itemRepositoryPath, itemPubArtifactCoordinates, packageName, packageMetadata);
                    }
                } catch (Exception e) {
                    log.warn("Reindex for package '{}' in storage '{}' repository '{}' error '{}'", path, storageId, repositoryId, ExceptionUtils.getStackTrace(e));
                }
            });
        } else {
            String packageMetadataFilePath = PubUtils.getPackageMetadataFilePath(packageMetadata.getName());
            RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
            Files.deleteIfExists(packageJsonRepositoryPath);
        }
        return packageMetadata;
    }

    private boolean shouldReindexPackage(PubIndexTypeEnum pubIndexTypeEnum) {
        return PubIndexTypeEnum.REINDEX.getType().equals(pubIndexTypeEnum.getType());
    }

    private void finalizePackageIndexing(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName, PubIndexTypeEnum pubIndexTypeEnum, PubPackageMetadata packageMetadata) throws Exception {
        List<PubPackageVersionMetadata> versions = packageMetadata.getVersions();
        if (CollectionUtils.isNotEmpty(versions)) {
            log.trace("Finished updating package '{}' metadata, package contains now {} versions", packageMetadata
                    .getName(), Integer.valueOf(versions.size()));
            Collections.sort(versions);
            packageMetadata.setLatest(versions.get(versions.size() - 1));
        }
        writeOrDeletePackageMetadataFile(repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName, pubIndexTypeEnum, packageMetadata);
    }

    private void indexType(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName, PubIndexTypeEnum pubIndexTypeEnum, PubPackageMetadata packageMetadata) {
        try {
            switch (pubIndexTypeEnum.getType()) {
                case "add":
                    handleAddPubPackage(packageMetadata, repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName);
                    break;
                case "delete":
                    handleDeletePubPackage(packageMetadata, repositoryPath, pubArtifactCoordinates, storageId, repositoryId, packageName);
                    break;
                default:
                    ;
            }
        } catch (Exception e) {
            log.warn("Error occurred during index processing error [{}]", ExceptionUtils.getStackTrace(e));
        }
    }

    private void writeOrDeletePackageMetadataFile(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName, PubIndexTypeEnum pubIndexTypeEnum, PubPackageMetadata packageMetadata) throws Exception {
        String packageMetadataFilePath = PubUtils.getPackageMetadataFilePath(packageMetadata.getName());
        RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
        if (CollectionUtils.isNotEmpty(packageMetadata.getVersions())) {
            log.trace("Writing metadata to: '{}'", packageMetadata);
            try {
                Files.writeString(packageJsonRepositoryPath, JSON.toJSONString(packageMetadata, SerializerFeature.PrettyFormat, SerializerFeature.DisableCircularReferenceDetect));
            } catch (IOException e) {
                log.error("Could not write to '{}' because of the following [{}]", packageName, ExceptionUtils.getStackTrace(e));
            }
        } else {
            log.trace("Metadata for package contains no versions, deleting file in '{}'", packageJsonRepositoryPath);
            Files.deleteIfExists(packageJsonRepositoryPath);
        }
    }

    private void handleDeletePubPackage(PubPackageMetadata packageMetadata, RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.debug("Handling Delete for package: '{}', version: '{}'", packageName, pubArtifactCoordinates.getVersion());
        Pubspec artifactMetadata = Pubspec.builder().version(pubArtifactCoordinates.getVersion()).build();
        PubPackageVersionMetadata versionMetadata = createPackageVersion(artifactMetadata, "");
        removeVersionFromPackageMetadata(packageMetadata, versionMetadata);
    }

    private void handleAddPubPackage(PubPackageMetadata packageMetadata, RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        log.debug("Handling Add for package: '{}', version: '{}'", packageName, pubArtifactCoordinates.getVersion());
        addPubPackage(repositoryPath, pubArtifactCoordinates, packageName, packageMetadata);
    }

    private void addPubPackage(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String packageName, PubPackageMetadata packageMetadata) throws Exception {
        PubMetadataExtractor pubMetadataExtractor = new PubMetadataExtractor();
        Pubspec artifactMetadata = pubMetadataExtractor.extractPubSpec(repositoryPath);
        if (PubUtils.isPackageMetadataValidForIndexing(packageName, artifactMetadata.getVersion())) {
            String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
            URI uri = pubArtifactCoordinates.convertToResource(pubArtifactCoordinates);
            PubPackageVersionMetadata versionMetadata = createPackageVersion(artifactMetadata, repositoryBaseUrl + uri.toString());
            Artifact artifact = repositoryPath.getArtifactEntry();
            if (Objects.nonNull(artifact)) {
                versionMetadata.setArchiveSha256(artifact.getChecksums().getOrDefault(MessageDigestAlgorithms.SHA_256, ""));
            }
            addVersionToPackageMetaData(versionMetadata, packageMetadata);
        }
    }

    private void removeVersionFromPackageMetadata(PubPackageMetadata packageMetadata, PubPackageVersionMetadata version) {
        log.debug("Remove version {} from package {}", version.getVersion(), packageMetadata.getName());
        List<PubPackageVersionMetadata> versions = packageMetadata.getVersions();
        versions.remove(version);
    }

    private PubPackageVersionMetadata createPackageVersion(Pubspec artifactMetadata, String path) {
        return PubPackageVersionMetadata.builder().version(artifactMetadata.getVersion()).pubspec(artifactMetadata).retracted(false).archiveUrl(path).published(Instant.now().toString()).build();
    }

    private void addVersionToPackageMetaData(PubPackageVersionMetadata version, PubPackageMetadata packageMetadata) {
        log.info("Adding version '{}' to package metadata versions", version.getVersion());
        List<PubPackageVersionMetadata> versions = CollectionUtils.isEmpty(packageMetadata.getVersions()) ? Lists.newLinkedList() : packageMetadata.getVersions();
        versions.add(version);
        packageMetadata.setVersions(versions.stream().distinct().collect(Collectors.toList()));
    }

    @NotNull
    private PubPackageMetadata extractPackageMetadata(RepositoryPath repositoryPath, PubArtifactCoordinates pubArtifactCoordinates, String storageId, String repositoryId, String packageName) throws Exception {
        JSONObject data = pubService.packages(repositoryPath.getRepository(), packageName, "");
        if (Objects.nonNull(data)) {
            return JSONObject.parseObject(data.toJSONString(), PubPackageMetadata.class);
        }
        return PubPackageMetadata.builder().name(packageName).discontinued(false).build();
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}

