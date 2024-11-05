package com.veadan.folib.util;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

@Slf4j
public class RepositoryPathUtil {

    public static List<S3Path> getS3FiePaths(S3Path s3Path) throws Exception {
        List<S3Path> listFile = new ArrayList<S3Path>();
        List<S3Path> listDir = new ArrayList<S3Path>();
        if (Files.exists(s3Path) && !Files.isDirectory(s3Path)) {
            if (!exclude(s3Path.getFileName().toString())) {
                log.info("S3 file {}", s3Path.getFileName().toString());
                listFile.add(s3Path);
            }
        }
        S3Iterator s3Iterator = new S3Iterator(s3Path);
        while (s3Iterator.hasNext()) {
            S3Path s3PathTemp = s3Iterator.next();
            if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                if (!exclude(s3PathTemp.getFileName().toString())) {
                    listDir.add(s3PathTemp);
                }
            } else {
                if (!exclude(s3PathTemp.getFileName().toString())) {
                    log.info("S3 file {}", s3PathTemp);
                    listFile.add(s3PathTemp);
                }
            }
        }
        while (listDir.size() != 0) {
            S3Path currentPath = listDir.get(0);
            listDir.remove(currentPath);
            s3Iterator = new S3Iterator(currentPath);
            while (s3Iterator.hasNext()) {
                S3Path s3PathTemp = s3Iterator.next();
                if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                    if (!exclude(s3PathTemp.getFileName().toString())) {
                        listDir.add(s3PathTemp);
                    }
                } else {
                    if (!exclude(s3PathTemp.getFileName().toString())) {
                        log.info("S3 file {}", s3PathTemp);
                        listFile.add(s3PathTemp);
                    }
                }
            }
        }
        log.info("s3Path [{}]  文件数量：{}", s3Path.toUri().toString(), listFile.size());
        return listFile;
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param layout         布局
     * @param repositoryPath 路径
     * @return 列表
     * @throws IOException 异常
     */
    public static List<RepositoryPath> getPaths(String layout, RepositoryPath repositoryPath) throws IOException {
        return getPaths(layout, repositoryPath, Collections.emptyList());
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param layout               布局
     * @param repositoryPath       路径
     * @param excludeDirectoryList 排除的目录列表
     * @param beginDate            开始日期
     * @param endDate              结束日期
     * @return 列表
     * @throws IOException 异常
     */
    public static List<RepositoryPath> getPaths(String layout, RepositoryPath repositoryPath, List<String> excludeDirectoryList, LocalDateTime beginDate, LocalDateTime endDate) throws IOException {
        List<RepositoryPath> pathList = Lists.newArrayList();
        if (!Files.exists(repositoryPath)) {
            log.warn("Path [{}] not exists", repositoryPath);
            return pathList;
        }
        RepositoryPathResolver repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
        if (!Files.isDirectory(repositoryPath) && withinTimeFrame(repositoryPath, beginDate, endDate)) {
            //是一个文件
            pathList.add(repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath)));
            log.info("Path [{}] find paths [{}]", repositoryPath, pathList.size());
            return pathList;
        }
        final boolean isDockerLayout = ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(layout);
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    if (include(1, itemPath, isDockerLayout) && withinTimeFrame(itemPath, beginDate, endDate)) {
                        log.info("Find path [{}]", itemPath);
                        pathList.add(repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath)));
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) dir;
                    if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !include(2, itemPath, isDockerLayout) || (CollectionUtils.isNotEmpty(excludeDirectoryList) && excludeDirectoryList.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                        log.info("RepositoryPath [{}] skip...", itemPath.toString());
                        return FileVisitResult.SKIP_SUBTREE;
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
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
        log.info("Path [{}] find paths [{}]", repositoryPath, pathList.size());
        return pathList;
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param layout               布局
     * @param repositoryPath       路径
     * @param excludeDirectoryList 排除的目录列表
     * @return 列表
     * @throws IOException 异常
     */
    public static List<RepositoryPath> getPaths(String layout, RepositoryPath repositoryPath, List<String> excludeDirectoryList) throws IOException {
        return getPaths(layout, repositoryPath, excludeDirectoryList, null, null);
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param repositoryPath 路径
     */
    public static List<RepositoryPath> getDockerImagePaths(RepositoryPath repositoryPath) {
        String blobs = "blobs", manifest = "manifest", temp = ".temp";
        List<String> excludeList = Lists.newArrayList(blobs, manifest, temp);
        List<RepositoryPath> pathList = Lists.newArrayList();
        if (!Files.exists(repositoryPath)) {
            log.warn("Path [{}] not exists", repositoryPath);
            return pathList;
        }
        if (!Files.isDirectory(repositoryPath)) {
            log.warn("Path [{}] not is directory", repositoryPath);
            return pathList;
        }
        try {
            RepositoryPathResolver repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    log.debug("Current path [{}]", itemPath);
                    if (include(1, itemPath, true) && !pathList.contains(itemPath.getParent().getParent())) {
                        log.info("Find image path [{}]", itemPath);
                        pathList.add(repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath.getParent().getParent())));
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) dir;
                    log.debug("Current directory path [{}]", itemPath);
                    if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !include(2, itemPath, true) || (CollectionUtils.isNotEmpty(excludeList) && excludeList.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                        log.debug("RepositoryPath [{}] skip...", itemPath.toString());
                        return FileVisitResult.SKIP_SUBTREE;
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
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
        log.info("Path [{}] find image paths [{}]", repositoryPath, pathList.size());
        return pathList;
    }

    public static boolean exclude(String name) {
        if (StringUtils.isBlank(name)) {
            return true;
        }
        if (name.startsWith(".") && name.endsWith(".metadata")) {
            return true;
        }
        if (name.startsWith(".") && name.endsWith(".foLibrary-metadata")) {
            return true;
        }
        String dsStore = ".DS_Store";
        if (name.endsWith(dsStore)) {
            return true;
        }
        return false;
    }

    public static boolean include(int type, RepositoryPath repositoryPath, boolean isDockerLayout) throws IOException {
       return include(type, repositoryPath, isDockerLayout, true);
    }

    public static boolean include(int type, RepositoryPath repositoryPath, boolean isDockerLayout, boolean filterArtifact) throws IOException {
        String name = repositoryPath.getFileName().toString();
        if (StringUtils.isBlank(name)) {
            return false;
        }
        if (RepositoryFiles.isHidden(repositoryPath)) {
            return false;
        }
        if (RepositoryFiles.isArtifactMetadata(repositoryPath)) {
            return false;
        }
        String dsStore = ".DS_Store";
        if (name.endsWith(dsStore)) {
            return false;
        }
        if (type == 1 && RepositoryFiles.isChecksum(repositoryPath)) {
            return false;
        }
        if (type == 1 && isDockerLayout && !name.startsWith("sha256")) {
            return false;
        }
        if (filterArtifact && type == 1 && !RepositoryFiles.isArtifact(repositoryPath)) {
            return false;
        }
        return true;
    }

    public static boolean isDockerTag(RepositoryPath path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String fullPath = path.toString();
            String relativizePath = RepositoryFiles.relativizePath(path);
            int deepSize = relativizePath.split("/").length;
            int two = 2;
            if (deepSize < two) {
                return false;
            }
            if (Files.isDirectory(path)) {
                try (Stream<Path> pathStream = Files.list(path)) {
                    return pathStream.anyMatch(RepositoryPathUtil::isManifestPath);
                }
            }
            String name = path.getFileName().toString();
            return name.startsWith(GlobalConstants.SHA_256) && !name.endsWith(GlobalConstants.CHECKSUM_SHA_256) && !name.endsWith(GlobalConstants.SELF_METADATA) && !name.endsWith(GlobalConstants.FO_LIBRARY_METADATA) && !fullPath.contains("blobs/sha256") && !fullPath.contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean isManifestPath(Path path) {
        try {
            if (Objects.isNull(path) || Files.notExists(path) || Files.isDirectory(path) || RepositoryFiles.isHidden(path)) {
                return false;
            }
            String name = path.getFileName().toString();
            return name.startsWith(GlobalConstants.SHA_256) && !name.endsWith(GlobalConstants.CHECKSUM_SHA_256) && !name.endsWith(GlobalConstants.SELF_METADATA) && !name.endsWith(GlobalConstants.FO_LIBRARY_METADATA) && !path.toString().contains("blobs/sha256") && !path.toString().contains("manifest/sha256");
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public static boolean withinTimeFrame(RepositoryPath repositoryPath, LocalDateTime beginDate, LocalDateTime endDate) {
        if (Objects.isNull(beginDate) || Objects.isNull(endDate)) {
            return true;
        }
        boolean flag = false;
        LocalDateTime fileCreationTime = getFileCreationTime(repositoryPath);
        if (Objects.isNull(fileCreationTime)) {
            return false;
        }
        if (fileCreationTime.isAfter(beginDate) && fileCreationTime.isBefore(endDate)) {
            flag = true;
        }
        log.info("RepositoryPath [{}] fileCreationTime [{}] beginDateTime [{}] endDateTime [{}] within time frame [{}]", repositoryPath.toString(), fileCreationTime, beginDate, endDate, flag);
        return flag;
    }

    public static LocalDateTime getFileCreationTime(Path path) {
        LocalDateTime lastModifiedDateTime = null;
        try {
            BasicFileAttributes attributes = Files.readAttributes(path, BasicFileAttributes.class);
            FileTime fileTime = attributes.creationTime();
            // 将FileTime转换为Instant
            Instant instant = fileTime.toInstant();
            // 将Instant转换为LocalDateTime
            lastModifiedDateTime = instant.atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime();
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return lastModifiedDateTime;
    }
}
