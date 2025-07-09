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

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.folib.io.*;
import com.google.common.collect.Maps;
import com.folib.artifact.ArtifactNotFoundException;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.commons.io.reloading.FSReloadableInputStreamHandler;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.enums.ProductTypeEnum;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.event.repository.RepositoryEventListenerRegistry;
import com.folib.io.LazyOutputStream.OutputStreamSupplier;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.StorageFileSystemProvider;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repositories.ArtifactRepository;
import com.folib.repositories.VulnerabilityRepository;
import com.folib.storage.ArtifactResolutionException;
import com.folib.storage.repository.Repository;
import com.folib.util.CommonUtils;
import com.folib.util.LocalDateTimeInstance;
import com.folib.util.RepositoryPathUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.spi.FileSystemProvider;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;

/**
 * This class decorates {@link StorageFileSystemProvider} with common layout specific
 * logic. <br>
 *
 * @author veadan
 * @see LayoutProvider
 */
public abstract class LayoutFileSystemProvider extends StorageFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(LayoutFileSystemProvider.class);

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Inject
    private ArtifactRepository artifactEntityRepository;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private VulnerabilityRepository vulnerabilityRepository;


    public LayoutFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    protected abstract AbstractLayoutProvider getLayoutProvider();

    @Override
    public LazyInputStream newInputStream(Path path,
                                          OpenOption... options)
            throws IOException {
        return new LazyInputStream(() -> {
            try {
                if (!Files.exists(path)) {
                    throw new ArtifactNotFoundException(path.toUri());
                }

                if (Files.isDirectory(path)) {
                    throw new ArtifactNotFoundException(path.toUri(),
                            String.format("The artifact path is a directory: [%s]",
                                    path.toString()));
                }

                ByteRangeInputStream bris = new ByteRangeInputStream(super.newInputStream(path, options));
                bris.setReloadableInputStreamHandler(new FSReloadableInputStreamHandler(path));
                bris.setLength(Files.size(path));

                return decorateStream((RepositoryPath) path, bris);
            } catch (NoSuchAlgorithmException e) {
                throw new IOException(e);
            }
        });
    }

    protected LayoutInputStream decorateStream(RepositoryPath path,
                                               InputStream is)
            throws NoSuchAlgorithmException, IOException {
        // Add digest algorithm only if it is not a Checksum (we don't need a Checksum of Checksum).
        if (Boolean.TRUE.equals(RepositoryFiles.isChecksum(path))) {
            return new LayoutInputStream(is, Collections.emptySet());
        }

        return new LayoutInputStream(is, path.getFileSystem().getDigestAlgorithmSet());
    }

    public RepositoryPath getChecksumPath(RepositoryPath path,
                                          String digestAlgorithm) {
        String checksumExtension = ".".concat(digestAlgorithm.toLowerCase().replaceAll("-", ""));

        return path.resolveSibling(path.getFileName().toString().concat(checksumExtension));
    }

    @Override
    public LazyOutputStream newOutputStream(Path path,
                                            OpenOption... options)
            throws IOException {
        return new LazyOutputStream(() -> {
            if (Files.exists(path) && Files.isDirectory(path)) {
                throw new ArtifactResolutionException(String.format("The artifact path is a directory: [%s]",
                        path.toString()));
            }
            if(!path.toString().startsWith("s3")){
                Files.createDirectories(path.getParent());
            }
            try {
                return decorateStream((RepositoryPath) path, super.newOutputStream(path, options));
            } catch (NoSuchAlgorithmException e) {
                throw new IOException(e);
            }
        });
    }

    protected LayoutOutputStream decorateStream(RepositoryPath path,
                                                OutputStream os)
            throws NoSuchAlgorithmException, IOException {
        Set<String> digestAlgorithmSet = path.getFileSystem().getDigestAlgorithmSet();
        LayoutOutputStream result = new LayoutOutputStream(os);

        // Add digest algorithm only if it is not a Checksum (we don't need a Checksum of Checksum).
        if (Boolean.TRUE.equals(RepositoryFiles.isChecksum(path))) {
            return result;
        }

        digestAlgorithmSet.stream()
                .forEach(e -> {
                    try {
                        result.addAlgorithm(e);
                    } catch (NoSuchAlgorithmException t) {
                        logger.error("Digest algorithm not supported: alg-[{}]", e, t);
                    }
                });
        return result;
    }

    private LocalDateTime getFileUpdateTime(RepositoryPath repositoryPath) {
        LocalDateTime lastModifiedDateTime = null;
        try {
            BasicFileAttributes attributes = Files.readAttributes(repositoryPath, BasicFileAttributes.class);
            FileTime fileTime = attributes.lastModifiedTime();
            // 将FileTime转换为Instant
            Instant instant = fileTime.toInstant();
            // 将Instant转换为LocalDateTime
            lastModifiedDateTime = instant.atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime();
        } catch (IOException ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        return lastModifiedDateTime;
    }

    public void storeChecksum(RepositoryPath basePath, String lastModifiedTime,
                              boolean forceRegeneration)
            throws IOException {
        final boolean isDockerLayout = ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(basePath.getRepository().getLayout());
        try {
            Files.walkFileTree(basePath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) file;
                        if (RepositoryPathUtil.include(1, itemPath, isDockerLayout, false, "")) {
                            if (StringUtils.isNotBlank(lastModifiedTime)) {
                                LocalDateTime lastModifiedDateTime = getFileUpdateTime(itemPath);
                                if (Objects.isNull(lastModifiedDateTime) || !LocalDateTime.now().minusDays(Integer.parseInt(lastModifiedTime)).isBefore(lastModifiedDateTime)) {
                                    logger.info("RepositoryPath path [{}] lastModifiedDateTime [{}] is before lastModifiedTime [{}] skip...", itemPath.toString(), lastModifiedTime, DateUtil.format(lastModifiedDateTime, DatePattern.NORM_DATETIME_PATTERN));
                                    return FileVisitResult.SKIP_SUBTREE;
                                }
                            }
                            logger.info("Find path [{}]", itemPath);
                            writeChecksum(itemPath, forceRegeneration);
                        }
                    } catch (Exception e) {
                        logger.error("Failed to write checksum for [{}]", file, e);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) dir;
                        logger.info("RepositoryPath directory [{}] ", itemPath.toString());
                        if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, isDockerLayout, "")) {
                            logger.info("RepositoryPath directory [{}] skip...", itemPath.toString());
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                    } catch (Exception e) {
                        logger.error("Failed to directory for [{}]", dir, e);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                                                          IOException exc)
                        throws IOException {
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    protected void writeChecksum(RepositoryPath path,
                                 boolean force)
            throws IOException {
        long startTime = System.currentTimeMillis();
        try (InputStream is = newInputStream(path)) {
            byte[] buffer = new byte[8192];
            long readStartTime = System.currentTimeMillis();
            while (is.read(buffer) > 0) {
                //calculate checksum while reading the stream
            }
            logger.info("RepositoryPath [{}] calculate digest take time [{}] ms", path, System.currentTimeMillis() - readStartTime);
            String layout = path.getRepository().getLayout();
            final Artifact artifact = artifactEntityRepository.findOneArtifact(path.getStorageId(), path.getRepositoryId(), RepositoryFiles.relativizePath(path));
            Set<String> digestAlgorithmSet = path.getFileSystem().getDigestAlgorithmSet();
            final Map<String, String> checksumMap = Maps.newHashMap();
            digestAlgorithmSet.forEach(p ->
            {
                String checksum = StreamUtils.findSource(LayoutInputStream.class, is).getMessageDigestAsHexadecimalString(p, layout);
                RepositoryPath checksumPath = getChecksumPath(path, p);
                boolean checksumPathExists = Files.exists(checksumPath);
                if (checksumPathExists && !force) {
                    return;
                }
                try {
                    String oldDigest = "";
                    if (checksumPathExists) {
                        oldDigest = Files.readString(checksumPath);
                    }
                    long digestWriteStartTime = System.currentTimeMillis();
                    Files.write(checksumPath, checksum.getBytes());
                    checksumMap.put(p, checksum);
                    logger.info("RepositoryPath [{}] algorithm [{}] digest [{}] oldDigest [{}] equals {} write take time [{}] ms", path, p, checksum, oldDigest, checksum.equals(oldDigest), System.currentTimeMillis() - digestWriteStartTime);
                } catch (IOException e) {
                    logger.error("Failed to write checksum for [{}]",
                            checksumPath.toString(), e);
                }
            });
            try {
                if (Objects.nonNull(artifact) && MapUtils.isNotEmpty(checksumMap) && !CommonUtils.areMapsEqual(artifact.getChecksums(), checksumMap)) {
                    logger.info("Artifact storageId [{}] repositoryId [{}] path [{}] checksums update old checksums [{}] new checksums [{}]", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), artifact.getChecksums(), checksumMap);
                    artifact.setChecksums(checksumMap);
                    artifact.setSizeInBytes(Files.size(path));
                    artifact.setLastUpdated(LocalDateTimeInstance.now());
                    artifactEntityRepository.saveOrUpdate(artifact);
                }
            } catch (Exception ex) {
                logger.error("Update artifact checksums for [{}] error [{}]",
                        path.toString(), ExceptionUtils.getStackTrace(ex));
            }
            logger.info("RepositoryPath [{}] new checksums [{}] take time [{}] ms", path, checksumMap, System.currentTimeMillis() - startTime);
        }
    }

    @Override
    public void delete(Path path,
                       boolean force)
            throws IOException {
        logger.info("Deleting in ({})...", path);
        RepositoryPath repositoryPath = (RepositoryPath) path;
        deleteMetadata(repositoryPath);
        boolean exists = Files.exists(path);
        boolean directory = Files.isDirectory(path);
        deleteArtifactMedataFile(repositoryPath, force);
        super.delete(path, force);
        if (!exists) {
            return;
        }
        if (!directory) {
            artifactEventListenerRegistry.dispatchArtifactPathDeletedEvent(path);
        } else {
            artifactEventListenerRegistry.dispatchArtifactDirectoryPathDeletedEvent(path);
        }
        logger.info("Deleted [{}]", path);
    }

    @Override
    protected void doDeletePath(RepositoryPath repositoryPath,
                                boolean force)
            throws IOException {
        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            super.doDeletePath(repositoryPath, force);
            return;
        }

        Artifact artifactEntry = Optional.ofNullable(repositoryPath.getArtifactEntry())
                .orElseGet(() -> fetchArtifactEntry(repositoryPath));
        if (artifactEntry != null) {
            artifactEntityRepository.delete(artifactEntry, repositoryPath.getRepository().getLayout());
            ArtifactCoordinates c = RepositoryFiles.readCoordinates(repositoryPath);
            if (Objects.nonNull(c) && StringUtils.isNotBlank(c.getId()) && ProductTypeEnum.Npm.getFoLibraryName().equals(repositoryPath.getRepository().getLayout())) {
                ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(artifactEntry.getStorageId(), artifactEntry.getRepositoryId(), c.getId());
                artifactIdGroup = artifactIdGroupRepository.findByArtifactIdGroup(artifactIdGroup.getUuid());
                if (Objects.nonNull(artifactIdGroup) && StringUtils.isNotBlank(artifactIdGroup.getMetadata())) {
                    artifactIdGroup.setMetadata("");
                    artifactIdGroupRepository.saveOrUpdate(artifactIdGroup);
                }
            }
            if (CollectionUtils.isNotEmpty(artifactEntry.getVulnerabilities())) {
                vulnerabilityRepository.asyncHandlerVulnerabilityForArtifactDelete(repositoryPath, artifactEntry.getVulnerabilities());
            }
        }

        super.doDeletePath(repositoryPath, force);
    }

    private Artifact fetchArtifactEntry(RepositoryPath repositoryPath) {
        Repository repository = repositoryPath.getRepository();
        String path;
        try {
            path = RepositoryFiles.relativizePath(repositoryPath);
        } catch (IOException e) {
            logger.error("Failed to fetch ArtifactEntry for [{}]", repositoryPath, e);
            return null;
        }

        return artifactEntityRepository.findOneArtifact(repository.getStorage().getId(),
                repository.getId(),
                path);
    }
    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryRelativePath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException {
        return getLayoutProvider().getRepositoryFileAttributes(repositoryRelativePath, attributeTypes);
    }

    protected void deleteMetadata(RepositoryPath repositoryPath)
            throws IOException {

    }

    public class PathOutputStreamSupplier implements OutputStreamSupplier {
        private Path path;

        private OpenOption[] options;

        public PathOutputStreamSupplier(Path path,
                                        OpenOption... options) {
            this.path = path;
            this.options = options;
        }

        @Override
        public OutputStream get() throws IOException {
            return LayoutFileSystemProvider.super.newOutputStream(unwrap(path), options);
        }

    }

    private void deleteArtifactMedataFile(RepositoryPath repositoryPath, boolean force) {
        try {
            if (Files.exists(repositoryPath) && Files.isSameFile(repositoryPath.getRoot(), repositoryPath)) {
                return;
            }
            String artifactMetadataFileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
            RepositoryPath artifactRepositoryPath = repositoryPath.getParent().resolve(artifactMetadataFileName);
            if (Files.exists(artifactRepositoryPath)) {
                super.delete(artifactRepositoryPath, force);
            }
            String artifactMetadataDirectoryName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".foLibrary-metadata";
            RepositoryPath artifactMetadataDirectoryPath = repositoryPath.getParent().resolve(artifactMetadataDirectoryName);
            if (Files.exists(artifactMetadataDirectoryPath)) {
                super.delete(artifactMetadataDirectoryPath, force);
            }
        } catch (Exception ex) {
            logger.error("删除制品缓存元数据文件 [{}] 失败：[{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
    }

}
