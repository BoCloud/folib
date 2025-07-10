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
package com.folib.providers.layout;

import cn.hutool.crypto.digest.SM3;
import com.folib.artifact.archive.*;
import com.google.common.collect.ImmutableSet;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ArtifactGroup;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.LayoutCoordinatesEntity;
import com.folib.providers.io.LayoutFileSystem;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CacheUtil;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.web.util.UriComponentsBuilder;

import jakarta.inject.Inject;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
public abstract class AbstractLayoutProvider<T extends LayoutCoordinatesEntity>
        implements LayoutProvider<T> {

    private static final Logger logger = LoggerFactory.getLogger(AbstractLayoutProvider.class);

    private static final ArchiveListingFunction ARCHIVE_LISTING_FUNCTION = new CompositeArchiveListingFunction(
            ImmutableSet.of(
                    ZipArchiveListingFunction.INSTANCE,
                    TarGzArchiveListingFunction.INSTANCE,
                    TarArchiveListingFunction.INSTANCE,
                    Bzip2ArchiveListingFunction.INSTANCE,
                    JarArchiveListingFunction.INSTANCE
            )
    );
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    @Lazy
    @Inject
    protected StorageProviderRegistry storageProviderRegistry;
    @Lazy
    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    public abstract Set<String> getDefaultArtifactCoordinateValidators();

    protected abstract boolean isArtifactMetadata(RepositoryPath repositoryPath);

    public abstract T getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException;

    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, SM3.ALGORITHM_NAME)
                .collect(Collectors.toSet());
    }

    public boolean isChecksum(RepositoryPath repositoryPath) {
        return isChecksum(repositoryPath.getFileName().toString());
    }

    public boolean isChecksum(String fileName) {
        for (String e : getDigestAlgorithmSet()) {
            if (fileName.toString().endsWith("." + e.replaceAll("-", "").toLowerCase())) {
                return true;
            }
        }

        return false;
    }


    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        if (attributeTypes == null || attributeTypes.length == 0) {
            return Collections.emptyMap();
        }

        Map<RepositoryFileAttributeType, Object> result = new HashMap<>();
        for (RepositoryFileAttributeType repositoryFileAttributeType : attributeTypes) {
            Object value;
            switch (repositoryFileAttributeType) {
                default:
                    Map<RepositoryFileAttributeType, Object> attributesLocal;
                    value = null;

                    break;
                case CHECKSUM:
                    value = isChecksum(repositoryPath);

                    break;
                case TEMP:
                    value = repositoryPath.isAbsolute()
                            && repositoryPath.startsWith(repositoryPath.getFileSystem()
                            .getRootDirectory()
                            .resolve(LayoutFileSystem.TEMP));

                    break;
                case METADATA:
                    value = isArtifactMetadata(repositoryPath);

                    break;
                case ARTIFACT:
                    attributesLocal = getRepositoryFileAttributes(repositoryPath,
                            RepositoryFileAttributeType.CHECKSUM);

                    boolean isChecksum = Boolean.TRUE.equals(attributesLocal.get(RepositoryFileAttributeType.CHECKSUM));
                    boolean isDirectory = Files.isDirectory(repositoryPath);

                    value = !isChecksum && !isDirectory && !RepositoryFiles.isArtifactMetadata(repositoryPath);

                    break;
                case COORDINATES:
                    attributesLocal = getRepositoryFileAttributes(repositoryPath,
                            RepositoryFileAttributeType.ARTIFACT);

                    boolean isArtifact = Boolean.TRUE.equals(attributesLocal.get(RepositoryFileAttributeType.ARTIFACT));
                    if (!isArtifact) {
                        value = null;
                        break;
                    }

                    T artifactCoordinates = getArtifactCoordinates(repositoryPath);
                    artifactCoordinates.setUuid(artifactCoordinates.buildPath());

                    value = artifactCoordinates;
                    break;
                case RESOURCE_URL:
                    value = resolveResource(repositoryPath);

                    break;
                case ARTIFACT_PATH:
                    value = RepositoryFiles.relativizePath(repositoryPath);

                    break;

                case STORAGE_ID:
                    value = repositoryPath.getRepository().getStorage().getId();

                    break;

                case REPOSITORY_ID:
                    value = repositoryPath.getRepository().getId();

                    break;
                case EXPIRED:
                    value = false;

                    break;
                case REFRESH_CONTENT:
                    value = false;

                    break;

            }
            if (value != null) {
                result.put(repositoryFileAttributeType, value);
            }
        }

        return result;
    }

    public URL resolveResource(RepositoryPath repositoryPath)
            throws IOException {
        CacheUtil<String, URI> cacheUtil = CacheUtil.getInstance();
        String key = "uri";
        URI baseUri = cacheUtil.get(key);
        if (Objects.isNull(baseUri)) {
            baseUri = configurationManager.getBaseUri();
            cacheUtil.put(key, baseUri);
        }
        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        URI artifactResource = RepositoryFiles.resolveResource(repositoryPath);

        return UriComponentsBuilder.fromUri(baseUri)
                .pathSegment("storages", storage.getId(), repository.getId(), "/")
                .build()
                .toUri()
                .resolve(artifactResource)
                .toURL();
    }

    @Override
    public Set<String> listArchiveFilenames(final RepositoryPath repositoryPath) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.listFilenames(repositoryPath);
            } catch (IOException e) {
                logger.warn("Unable to list filenames in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return Collections.emptySet();
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByFileName(repositoryPath, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByFileName(repositoryPath, path, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) {
        if (ARCHIVE_LISTING_FUNCTION.supports(repositoryPath)) {
            try {
                return ARCHIVE_LISTING_FUNCTION.getContentByEqualsFileName(repositoryPath, path, fileName);
            } catch (IOException e) {
                logger.warn("Unable to file content in archive path {} using {}",
                        repositoryPath, ARCHIVE_LISTING_FUNCTION, e);
            }
        }
        return null;
    }

    @Override
    public Set<ArtifactGroup> getArtifactGroups(RepositoryPath path)
            throws IOException {
        if (!RepositoryFiles.isArtifact(path)) {
            return Collections.emptySet();
        }

        Repository repository = path.getFileSystem().getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates c = RepositoryFiles.readCoordinates(path);
        Optional<ArtifactIdGroup> artifactIdGroup = artifactIdGroupRepository.findAllArtifactsInGroup(storage.getId(),
                repository.getId(),
                c.getId());

        return artifactIdGroup.map(ArtifactGroup.class::cast).map(Collections::singleton).orElse(Collections.emptySet());
    }

    @Override
    public int refreshContentInterval(RepositoryPath repositoryPath) {
        String key = repositoryPath.getRepository().getLayout().toUpperCase() + "_REFRESH_CONTENT_INTERVAL";
        String refreshContentInterval = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(refreshContentInterval)) {
            return GlobalConstants.DEFAULT_REFRESH_CONTENT_INTERVAL;
        }
        return Integer.parseInt(refreshContentInterval);
    }

}
