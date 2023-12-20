package com.veadan.folib.providers.io;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.util.CacheUtil;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * This utility class contains common methods to work with {@link RepositoryPath}
 *
 * @author xuxinping
 * @see RepositoryPath
 * @see Files
 */
public abstract class RepositoryFiles {

    public static Boolean isChecksum(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.CHECKSUM));
    }

    public static Boolean isMetadata(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.METADATA));
    }

    public static Boolean isArtifactMetadata(RepositoryPath path) {
        String fileName = path.getFileName().toString();
        String parentFileName = path.getParent().getFileName().toString();
        boolean flag = false;
        if (fileName.startsWith(".") && fileName.endsWith(".metadata")) {
            flag = true;
        } else if (parentFileName.startsWith(".") && parentFileName.endsWith(".foLibrary-metadata")) {
            flag = true;
        }
        return flag;
    }

    public static Boolean isArtifactChecksum(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        List<String> checksumList = Lists.newArrayList(".md5", ".sha1", ".sha256", ".sha512");
        return checksumList.stream().anyMatch(name::endsWith);
    }

    public static Boolean isTrash(RepositoryPath path)
            throws IOException {
        return (Boolean) Files.getAttribute(path, formatAttributes(RepositoryFileAttributeType.TRASH));
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
        String schemePrefix = String.format("%s:", StorageFileSystemProvider.FOLIB_SCHEME);
        String attributesLocal = attributes.replace(schemePrefix, "").trim();
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

        if (isTrash(p)) {
            result = result.resolve(LayoutFileSystem.TRASH);
        } else if (isTemp(p)) {
            result = result.resolve(LayoutFileSystem.TEMP);
        }

        return result.relativize(p.toUri());
    }

    public static void undelete(RepositoryPath p)
            throws IOException {
        p.getFileSystem().provider().undelete(p);
    }

    public static TempRepositoryPath temporary(RepositoryPath p)
            throws IOException {
        return TempRepositoryPath.of(p);
    }

    public static RepositoryPath trash(RepositoryPath p)
            throws IOException {
        return p.getFileSystem().provider().getTrashPath(p);
    }

    public static String relativizePath(RepositoryPath p)
            throws IOException {
        if (p.path != null) {
            return p.path;
        }

        return p.path = relativizeUri(p).toString();
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
        if (repositoryPath.getTarget() instanceof S3Path) {
            CacheUtil<String, String> cacheUtil = CacheUtil.getInstance();
            String cacheRootPathDir = cacheUtil.get("ARTIFACT_CACHE_ROOT_PATH");
            if (StringUtils.isNotBlank(cacheRootPathDir)) {
                Path cacheRootPath = Path.of(cacheRootPathDir);
                String sourcePath = repositoryPath.toString();
                String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
                String prefix = String.format("/%s/%s/", storageId, repositoryId);
                String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
                Path cachePath = cacheRootPath.resolve(targetSubPath);
                exists = Files.exists(cachePath);
            }
        }
        if (!exists) {
            exists = Files.exists(repositoryPath);
        }
        return exists;
    }

    public static void deleteTrash(RepositoryPath repositoryPath)
            throws IOException {
        repositoryPath.getFileSystem().provider().deleteTrash(repositoryPath);
    }

    public static void undeleteTrash(RepositoryPath repositoryPath)
            throws IOException {
        repositoryPath.getFileSystem().provider().undelete(repositoryPath);
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

}
