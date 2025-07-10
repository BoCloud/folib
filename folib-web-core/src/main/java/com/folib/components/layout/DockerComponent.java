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
package com.folib.components.layout;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.schema2.Manifests;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.DirectoryListingService;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class DockerComponent {

    private static final String PREFIX = "sha256:";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private ArtifactResolutionService artifactResolutionService;

    @Inject
    @Lazy
    private ArtifactManagementService artifactManagementService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws IOException {
        log.debug("Get manifest param [{}]", repositoryPath);
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        if (Files.isDirectory(repositoryPath)) {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(fileContents)) {
                return null;
            }
            FileContent fileContent = fileContents.get(0);
            repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), fileContent.getArtifactPath());
        }
        if (!Files.exists(repositoryPath)) {
            return null;
        }
        DockerCoordinates dockerArtifactCoordinates = DockerCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        String imageName = dockerArtifactCoordinates.getName();
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = readManifest(repositoryPath);
        if (StringUtils.isBlank(manifestString)) {
            return null;
        }
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest/" + manifests.getDigest());
                manifestString = readManifest(manifestPath);
                if (StringUtils.isBlank(manifestString)) {
                    continue;
                }
                itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                itemImageManifest.setDigest(manifests.getDigest());
                imageManifestList.add(itemImageManifest);
            }
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }

    public List<LayerManifest> getImageLayers(RepositoryPath repositoryPath) {
        try {
            DockerCoordinates dockerArtifactCoordinates = DockerCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
            String imageName = dockerArtifactCoordinates.getName();
            List<LayerManifest> layerManifests = Lists.newArrayList();
            String manifestString = readManifest(repositoryPath);
            if (StringUtils.isBlank(manifestString)) {
                return null;
            }
            ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                layerManifests.addAll(imageManifest.getLayers());
            }
            if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
                //多架构镜像
                ImageManifest itemImageManifest = null;
                for (Manifests manifests : imageManifest.getManifests()) {
                    RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest/" + manifests.getDigest());
                    manifestString = readManifest(manifestPath);
                    if (StringUtils.isBlank(manifestString)) {
                        continue;
                    }
                    itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                    if (CollectionUtils.isNotEmpty(itemImageManifest.getLayers())) {
                        layerManifests.addAll(itemImageManifest.getLayers());
                    }
                }
            }
            return layerManifests;
        } catch (Exception ex) {
            log.error("获取镜像层级列表错误：{}", ExceptionUtils.getStackTrace(ex));
            return null;
        }
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

    /**
     * 获取仓库下的tag路径
     *
     * @param repositoryPath 仓库路径
     * @return tag路径
     * @throws IOException 异常
     */
    public RepositoryPath getManifestPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath path = null;
        List<String> excludeList = Lists.newArrayList("temp", ".temp");
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            List<Path> pathList = pathStream.filter(p -> {
                try {
                    RepositoryPath itemRepositoryPath = (RepositoryPath) p;
                    return p.getFileName().toString().startsWith(PREFIX) && !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) &&
                            !Files.isHidden(p) && !Files.isDirectory(p) && !RepositoryFiles.isChecksum(itemRepositoryPath) && !RepositoryFiles.isArtifactMetadata(itemRepositoryPath);
                } catch (IOException e) {
                    log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                    return false;
                }
            })
                    .sorted()
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(pathList)) {
                path = (RepositoryPath) pathList.get(0);
                path.setArtifact(repositoryPathResolver.resolve(path.getStorageId(), path.getRepositoryId(), RepositoryFiles.relativizePath(path)).getArtifactEntry());

            }
            log.info("Tag [{}] manifestRepositoryPath [{}]", repositoryPath, path);
        }
        return path;
    }

    /**
     * 获取manifest
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param imagePath    镜像路径
     * @param digestOrTag  digestOrTag
     * @return RepositoryPath tag或者manifest的RepositoryPath
     */
    public RepositoryPath resolveManifest(String storageId, String repositoryId, String imagePath, String digestOrTag) {
        imagePath = StringUtils.removeEnd(imagePath, GlobalConstants.SEPARATOR);
        digestOrTag = StringUtils.removeEnd(digestOrTag, GlobalConstants.SEPARATOR);
        boolean isTag = !digestOrTag.startsWith("sha256:");
        String artifactPath = String.format("%s/%s", imagePath, digestOrTag);
        if (!isTag) {
            artifactPath = String.format("%s/%s", DockerLayoutProvider.MANIFEST, digestOrTag);
        }
        RepositoryPath repositoryPath = null;
        try {
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            repositoryPath.setDisableRemote(true);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath) || RepositoryFiles.hasRefreshContent(repositoryPath)) {
                if (RepositoryTypeEnum.HOSTED.getType().equals(rootRepositoryPath.getRepository().getType())) {
                    return null;
                }
                RepositoryPath manifestRepositoryPath = handleManifest(rootRepositoryPath, imagePath, digestOrTag);
                if (Objects.nonNull(manifestRepositoryPath) && Files.exists(manifestRepositoryPath)) {
                    return manifestRepositoryPath;
                }
            }
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                return null;
            }
            if (isTag) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(fileContents)) {
                    FileContent fileContent = fileContents.get(0);
                    repositoryPath = repositoryPathResolver.resolve(fileContent.getStorageId(), fileContent.getRepositoryId(), fileContent.getArtifactPath());
                }
            }
            if (isTag) {
                repositoryPath.setArtifactPath(RepositoryFiles.relativizePath(repositoryPath));
            }
            repositoryPath.setDisableRemote(true);
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            return repositoryPath;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        return null;
    }

    private RepositoryPath handleManifest(RootRepositoryPath rootRepositoryPath, String imagePath, String digestOrTag) {
        RepositoryPath tempManifestRepositoryPath = null, tagRepositoryPath = null, manifestRepositoryPath = null;
        try {
            boolean isTag = !digestOrTag.startsWith("sha256:");
            String targetPath = "";
            if (isTag) {
                targetPath = imagePath + GlobalConstants.SEPARATOR + digestOrTag;
            } else {
                targetPath = imagePath + GlobalConstants.SEPARATOR + ".temp_" + digestOrTag;
            }
            //从远程仓库获取镜像对应的manifest信息
            String tempManifestPath = String.format("%s/%s", targetPath, "manifest.json");
            String targetUrl = String.format("%s/manifests/%s", imagePath, digestOrTag);
            tempManifestRepositoryPath = rootRepositoryPath.resolve(tempManifestPath);
            tempManifestRepositoryPath.setTargetUrl(targetUrl);
            tempManifestRepositoryPath.setArtifactPath(imagePath);
            tempManifestRepositoryPath = artifactResolutionService.resolvePath(tempManifestRepositoryPath);
            if (Objects.isNull(tempManifestRepositoryPath) || !Files.exists(tempManifestRepositoryPath)) {
                return null;
            }
            ImageManifest imageManifest = JSON.parseObject(Files.readString(tempManifestRepositoryPath), ImageManifest.class);
            Integer one = 1;
            if (one.equals(imageManifest.getSchemaVersion())) {
                log.warn("ImagePath [{}] digestOrTag [{}] manifest [{}} docker v1 version not currently supported", imagePath, digestOrTag, tempManifestRepositoryPath.getFileName().toString());
                return null;
            }
            MessageDigest shaDigest = MessageDigest.getInstance("SHA-256");
            //解析临时的manifest文件，生成 SHA-256 checksum
            String shaChecksum = "sha256:" + getFileChecksum(shaDigest, new ByteArrayInputStream(Files.readAllBytes(tempManifestRepositoryPath)));
            if (isTag) {
                //写入到tag目录下的manifest文件中，tag下只能存在一个manifest
                tagRepositoryPath = tempManifestRepositoryPath.resolveSibling(shaChecksum);
                provideArtifact(tagRepositoryPath);
                artifactManagementService.store(tagRepositoryPath, tempManifestRepositoryPath);
            }
            //写入到仓库根目录下的manifest文件中
            manifestRepositoryPath = tempManifestRepositoryPath.getRoot().resolve(DockerLayoutProvider.MANIFEST).resolve(shaChecksum);
            provideArtifact(manifestRepositoryPath);
            artifactManagementService.store(manifestRepositoryPath, tempManifestRepositoryPath);
            return manifestRepositoryPath;
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(tempManifestRepositoryPath) && Files.exists(tempManifestRepositoryPath)) {
                try {
                    RepositoryFiles.delete(tempManifestRepositoryPath, true);
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        return null;
    }

    public void handleLayers(RepositoryPath targetManifestPath) throws IOException {
        provideArtifact(targetManifestPath);
    }

    public void provideArtifact(RepositoryPath repositoryPath) throws IOException {
        Artifact artifact = Optional.ofNullable(repositoryPath.getArtifactEntry())
                .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                        RepositoryFiles.readCoordinates(repositoryPath)));
        repositoryPath.setArtifact(artifact);
    }

    public String getFileChecksum(MessageDigest digest, InputStream stream) throws IOException {
        //Get file input stream for reading the file content
        //FileInputStream fis = new FileInputStream(file);

        //Create byte array to read data in chunks
        byte[] byteArray = new byte[4096];
        int bytesCount = 0;

        //Read file data and update in message digest
        while ((bytesCount = stream.read(byteArray)) != -1) {
            digest.update(byteArray, 0, bytesCount);
        }

        //close the stream; We don't need it now.
        stream.close();

        //Get the hash's bytes
        byte[] bytes = digest.digest();

        //This bytes[] has bytes in decimal format;
        //Convert it to hexadecimal format
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < bytes.length; i++) {
            sb.append(Integer.toString((bytes[i] & 0xff) + 0x100, 16).substring(1));
        }

        //return complete hash
        return sb.toString();
    }

    /**
     * 获取仓库下的tag列表
     *
     * @param repositoryPath 仓库路径
     * @param excludeList    要排除的列表
     * @return 目录列表
     * @throws IOException 异常
     */
    public List<Path> getTags(RepositoryPath repositoryPath, List<String> excludeList)
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
}
