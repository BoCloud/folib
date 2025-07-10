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
package com.folib.eventlistener.scanner;

import cn.hutool.core.io.FileTypeUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.HexUtil;
import com.google.common.collect.Sets;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.layout.DockerComponent;
import com.folib.domain.Artifact;
import com.folib.enums.SafeLevelEnum;
import com.folib.event.AsyncEventListener;
import com.folib.event.artifact.ArtifactEvent;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerFileSystem;
import com.folib.scanner.enums.Priority;
import com.folib.scanner.service.SbomScannerService;
import com.folib.scanner.service.ScannerService;
import com.folib.scanner.task.ScannerTask;
import com.folib.schema2.LayerManifest;
import com.folib.services.ArtifactService;
import com.folib.services.DictService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 * 事件监听，处理漏洞扫描
 */
@Slf4j
@Component
public class ArtifactEventScannerListener {

    @Inject
    private ArtifactService artifactService;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DictService dictService;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private DockerComponent dockerComponent;

    @Inject
    @Lazy
    private ScannerService scannerService;

    @Inject
    @Lazy
    private SbomScannerService scanService;

    @Value("${folib.temp}")
    private String tempPath;

    private static final String temp = ".temp";

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) throws IOException {
        RepositoryPath repositoryPath = event.getPath();
        int source = (int) event.getSource();
        boolean validate = validateEventTypeAndLayoutSupports(event);
        if (!validate) {
            return;
        }
        log.debug("Start handler artifact scan [{}] path [{}]", ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source), repositoryPath);
        handle(repositoryPath, source);
    }

    public Artifact handle(RepositoryPath repositoryPath, int source) {
        if (!Files.exists(repositoryPath)) {
            try {
                if (Objects.nonNull(repositoryPath.getArtifactEntry())) {
                    handlerUnwantedScan(repositoryPath, source);
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
            return null;
        }
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            //docker布局
            boolean isStoredEvent = ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source;
            if (isStoredEvent) {
                Path path = repositoryPath.getTarget();
                return handlerDockerFilePath(repositoryPath, source);
            } else {
                return handlerScan(repositoryPath, source);
            }
        } else {
            //非docker布局
            return handlerScan(repositoryPath, source);
        }
    }


    /**
     * 处理本地或nsf存储docker布局制品事件
     *
     * @param repositoryPath 制品路径
     * @param source         事件类型
     */
    private Artifact handlerDockerFilePath(RepositoryPath repositoryPath, Integer source) {
        String filePath = repositoryPath.toAbsolutePath().toString();
        File file = FileUtil.file(filePath);
        //版本目录
        File parentFile = file.getParentFile();
        //获取图层中的digest列表
        List<String> digestList = getImageManifest(repositoryPath);
        //存放解压文件的目录路径
        String tempPath = parentFile.getPath() + File.separator + temp;
        int maxSize = 100;
        if (CollectionUtils.isNotEmpty(digestList)) {
            String blobsPath = "";
            Set<String> filePaths = Sets.newLinkedHashSet();
            for (String digest : digestList) {
                if (filePaths.size() >= maxSize) {
                    break;
                }
                blobsPath = repositoryPath.getRoot() + File.separator + "blobs" + File.separator + digest;
                handlerDockerBlobFile(repositoryPath, filePaths, blobsPath, tempPath);
            }
            return handlerScan(repositoryPath, source, filePaths);
        }
        return null;
    }

    private List<String> getImageManifest(RepositoryPath repositoryPath) {
        return Optional.ofNullable(dockerComponent.getImageLayers(repositoryPath)).orElse(Collections.emptyList()).stream().map(LayerManifest::getDigest).distinct().collect(Collectors.toList());
    }

    /**
     * 处理docker文件
     *
     * @param repositoryPath 制品信息
     * @param filePaths      路径集合
     * @param blobsPath      文件路径
     * @param tempPath       存放解压文件的目录路径
     */
    private void handlerDockerBlobFile(RepositoryPath repositoryPath, Set<String> filePaths, String blobsPath, String tempPath) {
        File file = new File(blobsPath);
        if (file.isDirectory() || !file.exists()) {
            log.warn("blobsPath：{} not exists", blobsPath);
            return;
        }
        //增加魔数类型
        FileTypeUtil.putFileType("1f8b08000000000000ff", "gz");
        try {
            String hex = HexUtil.encodeHexStr(IoUtil.readBytes(new FileInputStream(file), 28), true);
            log.debug("路径：{}，hex：{}", file.getAbsolutePath(), hex);
        } catch (Exception ex) {
            log.error("读取魔数类型失败：{}", ExceptionUtils.getStackTrace(ex));
        }
        String type = FileTypeUtil.getType(file);
        String gz = "gz";
        int maxSize = 100;
        if (gz.equals(type)) {
            log.debug("路径：{}，类型：{}", file.getAbsolutePath(), type);
            List<String> filePathList = readTarFile(file, tempPath);
            if (CollectionUtils.isNotEmpty(filePathList)) {
                Path path = repositoryPath.getTarget();
                for (String filePath : filePathList) {
                    if (filePaths.size() >= maxSize) {
                        log.info("Docker制品扫描结果过多超出限制 [{}] size [{}] maxSize [{}]", path.toString(), filePaths.size(), maxSize);
                        return;
                    }
                    filePaths.add(filePath);
                }
            }
        }
    }

    /**
     * 处理扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     */
    private Artifact handlerScan(RepositoryPath repositoryPath, int source) {
        Artifact artifact = null;
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
            try {
                artifact = repositoryPath.getArtifactEntry();
                if (artifact == null) {
                    log.debug("No [{}] for [{}].",
                            Artifact.class.getSimpleName(),
                            repositoryPath);

                    return null;
                }
                artifact.setSafeLevel(SafeLevelEnum.UN_SCAN.getLevel());
                Set<String> filePaths = Sets.newLinkedHashSet();
                filePaths.add(repositoryPath.toAbsolutePath().toString());
                artifact.setFilePaths(filePaths);
                handlerScannerTask(repositoryPath, artifact);
                artifactService.saveOrUpdateArtifact(artifact);
            } catch (Exception ex) {
                log.error("获取Artifact错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        return artifact;
    }

    /**
     * 处理扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     * @param filePaths      文件路径集合
     */
    private Artifact handlerScan(RepositoryPath repositoryPath, int source, Set<String> filePaths) {
        Artifact artifact = null;
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
            try {
                artifact = repositoryPath.getArtifactEntry();
                if (artifact == null) {
                    log.debug("No [{}] for [{}].",
                            Artifact.class.getSimpleName(),
                            repositoryPath);

                    return artifact;
                }
                artifact.setSafeLevel(SafeLevelEnum.UN_SCAN.getLevel());
                artifact.setFilePaths(filePaths);
                handlerScannerTask(repositoryPath, artifact);
                artifactService.saveOrUpdateArtifact(artifact);
            } catch (IOException ex) {
                log.error("获取Artifact错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        return artifact;
    }

    /**
     * 校验是否是需要处理的事件类型、布局下的制品文件
     *
     * @param event 制品事件
     * @return true 校验通过 false 校验不通过
     */
    private boolean validateEventTypeAndLayoutSupports(ArtifactEvent<RepositoryPath> event) {
        boolean flag = false;
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return false;
        }
        flag = validateArtifactEvent(artifactEventTypeEnum);
        log.debug("监听到制品事件：{}，path路径：{} 制品事件类型是否为需要处理的类型：{}", artifactEventTypeEnum, repositoryPath, flag);
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == source) {
            //删除制品目录后续不需要校验文件类型是否支持
            return true;
        }
        if (flag) {
            flag = artifactComponent.layoutSupportsForScan(repositoryPath);
            if (!flag) {
                handlerUnwantedScan(repositoryPath, source);
            }
        }
        return flag;
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }


    /**
     * 读取tar.gz 文件
     *
     * @param tarFile  文件信息
     * @param tempPath 存放解压文件的目录路径
     * @return 文件路径列表
     */
    public static List<String> readTarFile(File tarFile, String tempPath) {
        FileInputStream fileInputStream = null;
        GzipCompressorInputStream gzipCompressorInputStream = null;
        TarArchiveInputStream tarArchiveInputStream = null;
        List<String> pathList = Lists.newArrayList();
        try {
            fileInputStream = new FileInputStream(tarFile);
            gzipCompressorInputStream = new GzipCompressorInputStream(fileInputStream);
            tarArchiveInputStream = new TarArchiveInputStream(gzipCompressorInputStream);
            TarArchiveEntry entry = null;
            List<String> list = Arrays.asList("jar", "war", "ear", "zip", "tgz", "nupkg", "nuspec", "packages.config", "whl", "egg", "rpm", "tar", "exe", "podspec");
            File extractFolder = new File(tempPath);
            String type, fileName;
            while ((entry = tarArchiveInputStream.getNextTarEntry()) != null) {
                if (entry.isDirectory()) {
                    continue;
                }
                if (entry.getSize() > 0) {
                    type = FileUtil.getSuffix(entry.getName());
                    fileName = FileUtil.getName(entry.getName());
                    if (list.contains(type) || list.stream().anyMatch(fileName::equals)) {
                        File curFile = new File(extractFolder, entry.getName());
                        File parent = curFile.getParentFile();
                        if (!parent.exists()) {
                            parent.mkdirs();
                        }
                        try (FileOutputStream fileOutputStream = new FileOutputStream(curFile)) {
                            IOUtils.copy(tarArchiveInputStream, fileOutputStream);
                        }
                        pathList.add(curFile.getPath());
                        log.debug("文件名称：{}，文件类型：{}，生成文件路径：{}", entry.getName(), type, curFile.getPath());
                    }
                }
            }
        } catch (Exception e) {
            log.error("读取tar.gz文件失败：{}", ExceptionUtils.getStackTrace(e));
        } finally {
            try {
                if (Objects.nonNull(fileInputStream)) {
                    fileInputStream.close();
                }
                if (Objects.nonNull(gzipCompressorInputStream)) {
                    gzipCompressorInputStream.close();
                }
                if (Objects.nonNull(tarArchiveInputStream)) {
                    tarArchiveInputStream.close();
                }
            } catch (IOException ex) {
                log.error("关闭IO流失败：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        return pathList;
    }

    /**
     * 处理无需扫描逻辑
     *
     * @param repositoryPath 制品信息
     * @param source         事件类型
     */
    private void handlerUnwantedScan(RepositoryPath repositoryPath, int source) {
        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() != source && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
            try {
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (artifact == null) {
                    log.debug("No [{}] for [{}].",
                            Artifact.class.getSimpleName(),
                            repositoryPath);

                    return;
                }
                artifact.setSafeLevel(SafeLevelEnum.UNWANTED_SCAN.getLevel());
                artifactService.saveOrUpdateArtifact(artifact);
            } catch (IOException ex) {
                log.error("获取Artifact错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private void handlerScannerTask(RepositoryPath repositoryPath, Artifact artifact) {
        if (!scanService.validateRepositoryScan(repositoryPath.getStorageId(), repositoryPath.getRepositoryId())) {
            return;
        }
        String taskId = artifact.getUuid();
        ScannerTask scannerTask = new ScannerTask(Priority.HIGH.getValue(), taskId,
                () -> {
                    try {
                        scanService.syncScan(Collections.singletonList(artifact));
                    } catch (Exception ex) {
                        log.error("HandlerScannerTask taskId [{}] error [{}] ", taskId, ExceptionUtils.getStackTrace(ex));
                    }
                });
        scannerService.addTask(scannerTask);
    }
}
