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
package com.folib.providers.io;

import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * This utility class contains common methods to work with {@link RepositoryPath}
 *
 * @author veadan
 * @see RepositoryPath
 * @see Files
 */
@Slf4j
public abstract class RepositoryFiles {
    public final static String SCHEME_PREFIX = StorageFileSystemProvider.FOLIB_SCHEME + ":";

    public final static List<String> CHECK_SUM_LIST = Lists.newArrayList(".md5", ".sha1", ".sha256", ".sha512", ".sm3");

    public static Boolean isChecksum(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.CHECKSUM));
    }

    public static Boolean isMetadata(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.METADATA));
    }

    public static Boolean isArtifactMetadata(RepositoryPath path) {
        RepositoryPath root = path.getRoot();
        if (path.equals(root)) {
            return false;
        }
        String fileName = path.getFileName().toString();
        String parentFileName = path.getParent().getFileName().toString();
        boolean flag = false;
        if (fileName.startsWith(".") && fileName.endsWith(".metadata")) {
            flag = true;
        } else if (parentFileName.startsWith(".") && parentFileName.endsWith(".foLibrary-metadata")) {
            flag = true;
        } else if (parentFileName.endsWith(".artifactory-metadata")) {
            flag = true;
        }
        return flag;
    }

    public static Boolean isArtifactMetadataDirectory(RepositoryPath path) {
        RepositoryPath root = path.getRoot();
        if (path.equals(root)) {
            return false;
        }
        boolean flag = false;
        String fileName = path.getFileName().toString();
        if (fileName.startsWith(".") && fileName.endsWith(".foLibrary-metadata")) {
            flag = true;
        } else if (fileName.endsWith(".artifactory-metadata")) {
            flag = true;
        }
        return flag;
    }

    public static boolean canDeleteArtifactMetadata(RepositoryPath path) throws IOException {
        RepositoryPath root = path.getRoot();
        if (path.equals(root)) {
            return false;
        }
        RepositoryPath repositoryPath = null;
        String fileName = path.getFileName().toString();
        if (fileName.endsWith(RepositoryPathUtil.DS_STORE)) {
            return true;
        }
        String parentFileName = path.getParent().getFileName().toString();
        if (fileName.startsWith(".") && fileName.endsWith(".metadata")) {
            repositoryPath = path.resolveSibling(StringUtils.removeEnd(StringUtils.removeStart(fileName, "."), ".metadata"));
            return !Files.exists(repositoryPath);
        } else if (isChecksum(path)) {
            Optional<String> checksumOptional = CHECK_SUM_LIST.stream().filter(fileName::endsWith).findFirst();
            if (checksumOptional.isPresent()) {
                repositoryPath = path.resolveSibling(StringUtils.removeEnd(fileName, checksumOptional.get()));
                return !Files.exists(repositoryPath);
            }
        } else if (fileName.startsWith(".") && fileName.endsWith(".foLibrary-metadata")) {
            repositoryPath = path.resolveSibling(StringUtils.removeEnd(StringUtils.removeStart(fileName, "."), ".foLibrary-metadata"));
            return !Files.exists(repositoryPath);
        } else if (parentFileName.startsWith(".") && parentFileName.endsWith(".foLibrary-metadata")) {
            repositoryPath = path.getParent().resolveSibling(StringUtils.removeEnd(StringUtils.removeStart(parentFileName, "."), ".foLibrary-metadata"));
            return !Files.exists(repositoryPath);
        } else if (fileName.endsWith(".artifactory-metadata")) {
            repositoryPath = path.resolveSibling(StringUtils.removeEnd(fileName, ".artifactory-metadata"));
            return !Files.exists(repositoryPath);
        } else if (parentFileName.endsWith(".artifactory-metadata")) {
            repositoryPath = path.getParent().resolveSibling(StringUtils.removeEnd(parentFileName, ".artifactory-metadata"));
            return !Files.exists(repositoryPath);
        }
        return false;
    }

    public static Boolean isHidden(Path path) throws IOException {
        String fileName = path.getFileName().toString();
        return Files.isHidden(path) || fileName.startsWith(".");
    }

    public static Boolean isArtifactChecksum(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        return CHECK_SUM_LIST.stream().anyMatch(name::endsWith);
    }

    public static Boolean isTemp(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.TEMP));
    }

    public static Boolean isArtifact(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.ARTIFACT));
    }

    public static boolean wasModifiedAfter(RepositoryPath path,
                                           Instant timeLinePoint)
            throws IOException {
        final FileTime lastModifiedTime = Files.getLastModifiedTime(path);
        return lastModifiedTime.toInstant().isAfter(timeLinePoint);
    }

    public static Boolean hasExpired(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.EXPIRED));
    }

    public static Boolean hasRefreshContent(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.REFRESH_CONTENT));
    }

    public static ArtifactCoordinates readCoordinates(RepositoryPath path)
            throws IOException {
        return (ArtifactCoordinates) Files.getAttribute(path,
                formatAttributes(RepositoryFileAttributeType.COORDINATES));
    }

    public static URL readResourceUrl(RepositoryPath path)
            throws IOException {
        return (URL) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.RESOURCE_URL));
    }

    public static String formatAttributes(RepositoryFileAttributeType... attributeTypes) {
        if (attributeTypes == null) {
            return "folib:*";
        }
        StringJoiner sj = new StringJoiner(",");
        for (RepositoryFileAttributeType repositoryFileAttributeType : attributeTypes) {
            sj.add(repositoryFileAttributeType.getName());
        }
        return String.format("%s:%s", StorageFileSystemProvider.FOLIB_SCHEME, sj.toString());
    }

    public static Set<RepositoryFileAttributeType> parseAttributes(String attributes) {
        if (attributes == null) {
            return Collections.emptySet();
        }

        String attributesLocal = attributes.replace(SCHEME_PREFIX, "").trim();
        if (attributesLocal.equals("*")) {
            return Arrays.stream(RepositoryFileAttributeType.values())
                    .collect(Collectors.toSet());
        }
        return Arrays.stream(attributesLocal.split(","))
                .map(e -> RepositoryFileAttributeType.of(e))
                .collect(Collectors.toSet());
    }

    public static URI relativizeUri(RepositoryPath p)
            throws IOException {
        URI result = p.getFileSystem().getRootDirectory().toUri();

       if (isTemp(p)) {
            result = result.resolve(LayoutFileSystem.TEMP);
        }

        return result.relativize(p.toUri());
    }

    public static TempRepositoryPath temporary(RepositoryPath p)
            throws IOException {
        return TempRepositoryPath.of(p);
    }

    public static String relativizePath(RepositoryPath p)
            throws IOException {
        if (p.path != null) {
            return p.path;
        }

        return p.path = p.getPath();
    }

    public static String relativizeOriginalPath(RepositoryPath p)
            throws IOException {
        if (p.path != null) {
            return p.path;
        }

        return p.path = p.getOriginalPath();
    }

    public static URI resolveResource(RepositoryPath p)
            throws IOException {
        if (RepositoryFiles.isArtifact(p)) {
            ArtifactCoordinates c = RepositoryFiles.readCoordinates(p);

            return c.buildResource();
        }

        return relativizeUri(p);
    }

    public static boolean artifactExists(RepositoryPath repositoryPath)
            throws IOException {
        boolean exists = false;
        if (!exists) {
            exists = Files.exists(repositoryPath);
        }
        return exists;
    }

    public static void deleteEmptyDirectory(RepositoryPath repositoryPath)
            throws IOException {
        repositoryPath.getFileSystem().provider().deleteEmptyDirectory(repositoryPath);
    }

    public static void delete(RepositoryPath path,
                              boolean force)
            throws IOException {
        path.getFileSystem().provider().delete(path, force);
    }

    public static void delete(RepositoryPath path)
            throws IOException {
        Files.delete(path);
    }

    public static boolean validateChecksum(RepositoryPath repositoryPath, Path cachePath) {
        try {
            //对比checksum
            Set<String> digestAlgorithmSet = repositoryPath.getFileSystem().getDigestAlgorithmSet();
            String sourceDigest = "sourceDigest", cacheDigest = "cacheDigest";
            String digestAlgorithm = "sha1";
            if (CollectionUtils.isNotEmpty(digestAlgorithmSet)) {
                digestAlgorithm = digestAlgorithmSet.stream().findFirst().orElse("");
            }
            String extension = GlobalConstants.POINT + digestAlgorithm.replaceAll("-", "").toLowerCase();
            RepositoryPath digestRepositoryPath = repositoryPath.resolveSibling(repositoryPath.getFileName() + extension);
            if (Files.exists(digestRepositoryPath)) {
                sourceDigest = Files.readString(digestRepositoryPath);
            }
            Path digestCachePath = cachePath.resolveSibling(cachePath.getFileName() + extension);
            if (Files.exists(digestCachePath)) {
                cacheDigest = Files.readString(digestCachePath);
            }
            return sourceDigest.equals(cacheDigest);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    public static boolean validateChecksum(RepositoryPath repositoryPath, RepositoryPath targetRepositoryPath) {
        try {
            Set<String> digestAlgorithmSet = repositoryPath.getFileSystem().getDigestAlgorithmSet();
            String digestAlgorithm = "sha1";
            if (CollectionUtils.isNotEmpty(digestAlgorithmSet)) {
                digestAlgorithm = digestAlgorithmSet.stream().findFirst().orElse("");
            }
            String extension = GlobalConstants.POINT + digestAlgorithm.replaceAll("-", "").toLowerCase();
            //对比checksum
            String sourceDigestAlgorithm = "sourceDigestAlgorithm", targetDigestAlgorithm = "targetDigestAlgorithm";
            RepositoryPath sourceDigestAlgorithmRepositoryPath = repositoryPath.resolveSibling(repositoryPath.getFileName() + extension);
            if (Files.exists(sourceDigestAlgorithmRepositoryPath)) {
                sourceDigestAlgorithm = Files.readString(sourceDigestAlgorithmRepositoryPath);
            }
            Path targetDigestAlgorithmTargetPath = targetRepositoryPath.resolveSibling(targetRepositoryPath.getFileName() + extension);
            if (Files.exists(targetDigestAlgorithmTargetPath)) {
                targetDigestAlgorithm = Files.readString(targetDigestAlgorithmTargetPath);
            }
            return sourceDigestAlgorithm.equals(targetDigestAlgorithm);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    public static boolean isDirectoryEmpty(Path path) throws IOException {
        // 确保路径是目录
        if (!Files.isDirectory(path)) {
            return false;
        }
        // 检查目录是否为空
        try (DirectoryStream<Path> dirStream = Files.newDirectoryStream(path)) {
            return !dirStream.iterator().hasNext();
        }
    }
}
