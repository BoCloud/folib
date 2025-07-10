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

import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.DockerLayoutComponent;
import com.folib.enums.ProductTypeEnum;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.util.RepositoryPathUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;
import java.util.Objects;

/**
 * @author Veadan
 */
public class DockerFileSystemProvider
        extends LayoutFileSystemProvider {

    private static final Logger logger = LoggerFactory.getLogger(DockerFileSystemProvider.class);

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DockerLayoutProvider layoutProvider;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private DockerLayoutComponent dockerLayoutComponent;

    @Value("${folib.temp}")
    private String tempPath;

    public DockerFileSystemProvider(FileSystemProvider storageFileSystemProvider) {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider() {
        return layoutProvider;
    }

    @Override
    public void delete(Path path,
                       boolean force)
            throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) path;
        logger.info("Removing {}...", repositoryPath);
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        String rootArtifactPath = RepositoryFiles.relativizePath(repositoryPath.getRoot());
        handlerManifestAndBlob(repositoryPath, force);
        super.delete(repositoryPath, force);
        try {
            if (!artifactPath.equals(rootArtifactPath)) {
                RepositoryPath parent = repositoryPath.getParent();
                if (Files.exists(parent) && !Files.isSameFile(repositoryPath.getRoot(), parent) && DockerCoordinates.DOCKER_LAYER_DIR_NAME_LIST.stream().noneMatch(item -> item.equals(parent.getFileName().toString())) && RepositoryFiles.isDirectoryEmpty(parent)) {
                    Files.deleteIfExists(parent);
                    logger.info("Delete parent root path {}", parent.toString());
                }
            }
        } catch (Exception ex) {
            logger.warn("删除父目录失败 {}", ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath 路径
     * @param force          是否强制
     * @throws IOException io异常
     */
    public void handlerManifestAndBlob(RepositoryPath repositoryPath, boolean force) throws IOException {
        handlerManifestAndBlob(repositoryPath, force, null);
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath      路径
     * @param force               是否强制
     * @param currentManifestPath manifestPath
     * @throws IOException io异常
     */
    public void handlerManifestAndBlob(RepositoryPath repositoryPath, boolean force, Path currentManifestPath) throws IOException {
        if (!Files.isDirectory(repositoryPath)) {
            if (!DockerCoordinates.isDockerTag(repositoryPath)) {
                return;
            }
            currentManifestPath = repositoryPath;
        }
        List<Path> tagList = Lists.newArrayList();
        if (Objects.isNull(currentManifestPath)) {
            //当前版本下manifest文件信息
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    if (DockerCoordinates.isTagPath(file)) {
                        tagList.add(file);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                                                          IOException exc)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) dir;
                    if (!RepositoryPathUtil.include(2, itemPath, true, ProductTypeEnum.Docker.getFoLibraryName())) {
                        logger.debug("RepositoryPath [{}] skip...", itemPath.toString());
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } else {
            tagList.add(currentManifestPath);
        }
        if (CollectionUtils.isNotEmpty(tagList)) {
            for (Path tagPath : tagList) {
                //manifest目录下的当前版本的文件信息
                logger.info("Tag path [{}] ", tagPath.toString());
                boolean flag = checkRelation(tagPath.getFileName().toString(), (RepositoryPath) tagPath, 1);
                if (!flag) {
                    handlerLocalPath((RepositoryPath) tagPath, force);
                }
            }
        }
    }

    public void handlerLocalPath(RepositoryPath tagPath, boolean force) throws IOException {
        String manifest = "manifest", blobs = "blobs", storageId = tagPath.getStorageId(), repositoryId = tagPath.getRepositoryId();
        List<ImageManifest> currentManifestList = dockerLayoutComponent.getImageManifests(tagPath);
        if (CollectionUtils.isEmpty(currentManifestList)) {
            return;
        }
        for (ImageManifest itemImageManifest : currentManifestList) {
            RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, manifest + File.separator + itemImageManifest.getDigest());
            final boolean flag = dockerLayoutComponent.handleManifest(tagPath, manifestRepositoryPath, force);
            if (flag) {
                continue;
            }
            //当前版本下的配置信息
            if (Objects.nonNull(itemImageManifest.getConfig())) {
                String currentConfigDigest = itemImageManifest.getConfig().getDigest();
                final RepositoryPath configBlobRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, blobs + File.separator + currentConfigDigest);
                dockerLayoutComponent.handleBlob(configBlobRepositoryPath, force);
            }
            //manifest下的层级信息
            List<LayerManifest> currentLayerManifestList = itemImageManifest.getLayers();
            if (CollectionUtils.isNotEmpty(currentLayerManifestList)) {
                for (LayerManifest item : currentLayerManifestList) {
                    String blobArtifactPath = blobs + File.separator + item.getDigest();
                    final RepositoryPath blobRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, blobArtifactPath);
                    dockerLayoutComponent.handleBlob(blobRepositoryPath, force);
                }
            }
        }
    }

    /**
     * 校验当前版本的manifest文件是否被其他版本使用
     *
     * @param fileName       manifest名称
     * @param repositoryPath 当前文件
     * @param type           1 tag 2 manifest 3 blob
     * @return true 在使用 false 不在使用
     */
    public boolean checkRelation(String fileName, RepositoryPath repositoryPath, Integer type) {
        boolean existsRelation = true;
        try {
            String uuid = String.format("%s-%s-%s", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
            long count = artifactRepository.countDockerArtifactRelation(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), uuid, fileName, type);
            logger.info("RepositoryPath [{}] [{}] [{}] type [{}] uuid [{}] count [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath), type, uuid, count);
            existsRelation = count > 0;
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        return existsRelation;
    }
}
