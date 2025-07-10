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
package com.folib.components;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ProductTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.Manifests;
import com.folib.services.ArtifactManagementService;
import com.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * @date 2024/3/4
 **/
@Slf4j
@Component
public class DockerLayoutComponent {

    @Autowired
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    @Lazy
    private ArtifactManagementService artifactManagementService;

    /**
     * 获取仓库下的tag列表
     *
     * @param repositoryPath 仓库路径
     * @param excludeList    要排除的列表
     * @return 目录列表
     * @throws IOException 异常
     */
    private List<Path> getTags(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        List<Path> directoryList;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            directoryList = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) && RepositoryPathUtil.isDockerTag((RepositoryPath) p))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
        return directoryList;
    }

    public String readManifest(RepositoryPath manifestPath) {
        if (!DockerCoordinates.isManifestPath(manifestPath)) {
            log.warn(String.format("RepositoryPath [%s] not is a manifest path or not exists", manifestPath));
            return "";
        }
        try {
            return Files.readString(manifestPath);
        } catch (Exception ex) {
            log.warn("Read manifestPath [{}] error [{}]", manifestPath, ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws IOException {
        log.debug("Get manifest param [{}]", repositoryPath);
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        if (Files.isDirectory(repositoryPath)) {
            List<Path> tagList = getTags(repositoryPath, GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST);
            if (CollectionUtils.isEmpty(tagList)) {
                return null;
            }
            Path tag = tagList.get(0);
            RepositoryPath tagRepositoryPath = (RepositoryPath) tag;
            repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath));
        }
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        DockerCoordinates dockerArtifactCoordinates = DockerCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = readManifest(repositoryPath);
        if (StringUtils.isBlank(manifestString)) {
            return null;
        }
        ImageManifest imageManifest = parseImageManifest(repositoryPath, manifestString);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest/" + manifests.getDigest());
                manifestString = readManifest(manifestPath);
                if (StringUtils.isBlank(manifestString)) {
                    continue;
                }
                itemImageManifest = parseImageManifest(manifestPath, manifestString);
                itemImageManifest.setDigest(manifests.getDigest());
                imageManifestList.add(itemImageManifest);
            }
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }

    public boolean handleManifest(RepositoryPath tagPath, RepositoryPath manifestRepositoryPath, boolean force) {
        AtomicBoolean atomicBoolean = new AtomicBoolean(true);
        final Set<Boolean> flagSet = Sets.newConcurrentHashSet();
        try {
            if (RepositoryFiles.isChecksum(manifestRepositoryPath) || RepositoryFiles.isArtifactMetadata(manifestRepositoryPath) || !DockerCoordinates.isManifestPath(manifestRepositoryPath)) {
                return atomicBoolean.get();
            }
            log.debug("Manifest repositoryPath [{}] [{}] [{}]", manifestRepositoryPath.getStorageId(), manifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(manifestRepositoryPath));
            String manifestName = manifestRepositoryPath.getFileName().toString();
            //遍历仓库下所有的tag，判断是否有tag引用当前manifest
            Files.walkFileTree(manifestRepositoryPath.getRoot(), new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    if (DockerCoordinates.isTagPath(itemPath) && !Files.isSameFile(tagPath, itemPath)) {
                        log.debug("Tag repositoryPath [{}] [{}] [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                        List<ImageManifest> imageManifestList = getImageManifests(itemPath);
                        if (CollectionUtils.isNotEmpty(imageManifestList)) {
                            final boolean flag = imageManifestList.stream().filter(item -> StringUtils.isNotBlank(item.getDigest())).anyMatch(item -> manifestName.equals(item.getDigest()));
                            flagSet.add(flag);
                            if (flag) {
                                log.info("Tag repositoryPath [{}] [{}] [{}] find match manifest [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath), manifestName);
                                return FileVisitResult.TERMINATE;
                            }
                        }
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) dir;
                    if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, true, ProductTypeEnum.Docker.getFoLibraryName()) || (DockerCoordinates.DOCKER_LAYER_DIR_NAME_LIST.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
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
            atomicBoolean.set(flagSet.contains(true));
            if (!atomicBoolean.get()) {
                try {
                    //manifest不存在引用，删除
                    String artifactPath = RepositoryFiles.relativizePath(manifestRepositoryPath);
                    artifactManagementService.delete(manifestRepositoryPath, force);
                    log.info("Manifest repositoryPath [{}] [{}] [{}] is deleted", manifestRepositoryPath.getStorageId(), manifestRepositoryPath.getRepositoryId(), artifactPath);
                } catch (IOException e) {
                    log.warn(ExceptionUtils.getStackTrace(e));
                }
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return atomicBoolean.get();
    }

    public void handleBlob(RepositoryPath blobsRepositoryPath, boolean force) {
        try {
            String blobName = blobsRepositoryPath.getFileName().toString();
            if (RepositoryFiles.isChecksum(blobsRepositoryPath) || RepositoryFiles.isArtifactMetadata(blobsRepositoryPath) || !DockerCoordinates.include(blobName)) {
                return;
            }
            log.debug("Blob repositoryPath [{}] [{}] [{}]", blobsRepositoryPath.getStorageId(), blobsRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(blobsRepositoryPath));
            AtomicBoolean atomicBoolean = new AtomicBoolean(true);
            final Set<Boolean> flagSet = Sets.newConcurrentHashSet();
            //遍历仓库下的manifest目录，判断是否有manifest引用当前blob
            Files.walkFileTree(blobsRepositoryPath.getRoot().resolve(DockerLayoutProvider.MANIFEST), new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    if (DockerCoordinates.isRealManifestPath(itemPath)) {
                        log.debug("Manifest repositoryPath [{}] [{}] [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                        List<ImageManifest> imageManifestList = getImageManifests(itemPath);
                        if (CollectionUtils.isNotEmpty(imageManifestList)) {
                            final boolean flag = imageManifestList.stream().filter(item -> Objects.nonNull(item.getConfig())).map(item -> item.getConfig().getDigest()).anyMatch(blobName::equals) || imageManifestList.stream().filter(item -> CollectionUtils.isNotEmpty(item.getLayers())).flatMap(item -> item.getLayers().stream()).anyMatch(item -> blobName.equals(item.getDigest()));
                            flagSet.add(flag);
                            if (flag) {
                                log.info("Manifest repositoryPath [{}] [{}] [{}] find match blob [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath), blobName);
                                return FileVisitResult.TERMINATE;
                            }
                        }
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                                                          IOException exc)
                        throws IOException {
                    return FileVisitResult.CONTINUE;
                }
            });
            atomicBoolean.set(flagSet.contains(true));
            if (!atomicBoolean.get()) {
                try {
                    //blob不存在引用，删除
                    String artifactPath = RepositoryFiles.relativizePath(blobsRepositoryPath);
                    artifactManagementService.delete(blobsRepositoryPath, force);
                    log.info("Blob repositoryPath [{}] [{}] [{}] is deleted", blobsRepositoryPath.getStorageId(), blobsRepositoryPath.getRepositoryId(), artifactPath);
                } catch (Exception e) {
                    log.warn(ExceptionUtils.getStackTrace(e));
                }
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
    }

    private ImageManifest parseImageManifest(RepositoryPath repositoryPath, String manifestString) {
        try {
            return JSON.parseObject(manifestString, ImageManifest.class);
        } catch (Exception ex) {
            log.error("RepositoryPath [{}] manifest [{}] parse error [{}]", repositoryPath, manifestString, ExceptionUtils.getStackTrace(ex));
            throw ex;
        }
    }

}
