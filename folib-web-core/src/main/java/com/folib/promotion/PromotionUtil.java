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
package com.folib.promotion;

import cn.hutool.core.lang.UUID;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.DebianCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.layout.DockerComponent;
import com.folib.components.security.SecurityComponent;
import com.folib.config.PromotionConfig;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.DebianConstant;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.ArtifactPromotion;
import com.folib.domain.DockerSubsidiary;
import com.folib.domain.PromotionFileRelativePath;
import com.folib.dto.TargetRepositoyDto;
import com.folib.enums.DeltaIndexEventType;
import com.folib.enums.ProductTypeEnum;
import com.folib.event.DebianIndexEvent;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.indexer.DebianIncrementalIndexer;
import com.folib.indexer.DebianReleaseMetadataIndexer;
import com.folib.metadata.indexer.RpmRepoIndexer;
import com.folib.model.request.ArtifactSliceUploadReq;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.scanner.common.util.SpringContextUtil;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactMetadataService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactService;
import com.folib.services.ArtifactWebService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import com.folib.util.MessageDigestUtils;
import com.folib.util.RepositoryPathUtil;
import com.folib.utils.DockerUtils;
import com.folib.wrapper.BufferedInputStreamWrapper;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.InputStreamBody;
import org.apache.http.entity.mime.content.StringBody;
import org.mockito.internal.util.collections.Sets;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.FutureTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author veadan
 */
@Component
@Slf4j
public class PromotionUtil {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncCopyThreadPoolTaskExecutor;
    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private ArtifactService artifactService;

    @Autowired
    private ArtifactWebService artifactWebService;

    @Autowired
    private SecurityComponent securityComponent;

    @Autowired
    @Lazy
    private ArtifactComponent artifactComponent;

    @Autowired
    @Lazy
    private DockerComponent dockerComponent;
    @Autowired
    private PromotionConfig promotionConfig;
    @Autowired
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;
    @Autowired
    private DistributedCacheComponent distributedCacheComponent;
    @Autowired
    private ArtifactMetadataService artifactMetadataService;
    @Value("${folib.temp}")
    private String tempPath;

    //100MB
    private static final long MAX_SLICE_BYTE_SIZE = 100L;

    //1024M
    private static final long BUFFER_SIZE = 1024 * 1024 * 1024;

    public void executeSyncCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) {
        try {
            handleCopy(sourcePath, srcRepository, targetPath, targetRepository);
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath, ExceptionUtils.getStackTrace(e));
        }
    }

    public void executeFastSyncCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) {
        try {
            handleFastCopy(sourcePath, srcRepository, targetPath, targetRepository);
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath, ExceptionUtils.getStackTrace(e));
        }
    }

    public void executeFastSyncMove(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) {
        try {
            handleFastMove(sourcePath, srcRepository, targetPath, targetRepository);
            log.info("Execute move srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath);
        } catch (Exception e) {
            log.info("Execute move srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath, ExceptionUtils.getStackTrace(e));
        }
    }

    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executeCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) {
        try {
            handleCopy(sourcePath, srcRepository, targetPath, targetRepository);
            handleRpm(targetRepository);
            handleDebian(targetPath);
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath);
        } catch (Exception e) {
            log.info("Execute copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), sourcePath, ExceptionUtils.getStackTrace(e));
        }
    }


    @Async("asyncCopyThreadPoolTaskExecutor")
    public void executeMove(ArtifactPromotion artifactPromotion) {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        List<TargetRepositoyDto> list = artifactPromotion.getTargetRepositoyList();
        List<FutureTask<String>> listTask = Lists.newArrayList();
        list.forEach(target -> {
            // 多个目标仓库移动
            String targetStorageId = target.getTargetStorageId();
            String targetRepositoryId = target.getTargetRepositoryId();
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            RepositoryPath targetPath = getTargetPath(artifactPromotion, srcRepositoryPath, targetRepository);
            FutureTask<String> future = new FutureTask<String>(
                    new ArtifactPromotionCopyTask(srcPath, srcRepository, targetPath, targetRepository));
            listTask.add(future);
            asyncCopyThreadPoolTaskExecutor.submit(future);
        });
        boolean delFlag = true;
        for (FutureTask<String> task : listTask) {
            try {
                String rs = task.get();
                if (StringUtils.isNotBlank(rs)) {
                    delFlag = false;
                    log.error("Move error [{}]", rs);
                }
            } catch (Exception e) {
                log.error("error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        if (delFlag) {
            try {
                artifactManagementService.delete(srcRepositoryPath, true);
            } catch (IOException e) {
                log.error("Delete srcRepositoryPath error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        log.info("Execute move params [{}] finished", JSONObject.toJSONString(artifactPromotion));
    }

    public void executeSyncMove(ArtifactPromotion artifactPromotion) {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        List<TargetRepositoyDto> list = artifactPromotion.getTargetRepositoyList();
        List<FutureTask<String>> listTask = Lists.newArrayList();
        list.forEach(target -> {
            // 多个目标仓库移动
            String targetStorageId = target.getTargetStorageId();
            String targetRepositoryId = target.getTargetRepositoryId();
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            RepositoryPath targetPath = getTargetPath(artifactPromotion, srcRepositoryPath, targetRepository);
            FutureTask<String> future = new FutureTask<String>(
                    new ArtifactPromotionCopyTask(srcPath, srcRepository, targetPath, targetRepository));
            listTask.add(future);
            asyncCopyThreadPoolTaskExecutor.submit(future);
        });
        boolean delFlag = true;
        for (FutureTask<String> task : listTask) {
            try {
                String rs = task.get();
                if (StringUtils.isNotBlank(rs)) {
                    delFlag = false;
                    log.error("Move error [{}]", rs);
                }
            } catch (Exception e) {
                log.error("error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        if (delFlag) {
            try {
                artifactManagementService.delete(srcRepositoryPath, true);
            } catch (IOException e) {
                log.error("Delete srcRepositoryPath error [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        log.info("Execute move params [{}] finished", JSONObject.toJSONString(artifactPromotion));
    }

    public void handleCopy(RepositoryPath path, Repository srcRepository, Repository targetRepository) throws Exception {
        final String srcStorageId = srcRepository.getStorage().getId(), srcRepositoryId = srcRepository.getId(),
                targetStorageId = targetRepository.getStorage().getId(), targetRepositoryId = targetRepository.getId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(srcRepository.getLayout(), path);
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
        for (RepositoryPath srcRepositoryPath : list) {
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcRepositoryPath));
            if (!RepositoryFiles.isArtifact(srcRepositoryPath)) {
                log.info(String.format("RepositoryPath：%s not is artifact skip", srcRepositoryPath));
                continue;
            }
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            RepositoryPath targetBlobPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcBlobPath));
                            if (Files.exists(targetBlobPath) && RepositoryFiles.validateChecksum(srcBlobPath, targetBlobPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcBlobPath.toString(), targetBlobPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcBlobPath, targetBlobPath);
                            try (InputStream inputStream = Files.newInputStream(srcBlobPath)) {
                                artifactManagementService.store(targetBlobPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcBlobPath, targetBlobPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            RepositoryPath targetManiFestPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcMainFestPath));
                            if (Files.exists(targetManiFestPath) && RepositoryFiles.validateChecksum(srcMainFestPath, targetManiFestPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcMainFestPath.toString(), targetManiFestPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcMainFestPath, targetManiFestPath);
                            try (InputStream inputStream = Files.newInputStream(srcMainFestPath)) {
                                artifactManagementService.store(targetManiFestPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcMainFestPath, targetManiFestPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcRepositoryPath, targetRepositoryPath);
            try (InputStream is = Files.newInputStream(srcRepositoryPath)) {
                //同步metadata
                setMetaData(targetRepositoryPath, getMetaData(srcRepositoryPath));
                artifactManagementService.store(targetRepositoryPath, is);
            } catch (IOException e) {
                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcRepositoryPath, targetRepositoryPath, ExceptionUtils.getStackTrace(e));
                throw new Exception(e.getMessage());
            }
            handleMaven(targetRepositoryPath);
            if (isDocker) {
                List<DockerSubsidiary> dockerSubsidiaries = DockerUtils.getDockerSubsidiaryFilePaths(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(dockerSubsidiaries)) {
                    RepositoryPath srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath = null;
                    for (DockerSubsidiary dockerSubsidiary : dockerSubsidiaries) {
                        srcDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, dockerSubsidiary.getPath());
                        if (Files.exists(srcDockerSubsidiaryRepositoryPath)) {
                            targetDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, dockerSubsidiary.getPath());
                            if (Files.exists(targetDockerSubsidiaryRepositoryPath) && RepositoryFiles.validateChecksum(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcDockerSubsidiaryRepositoryPath.toString(), targetDockerSubsidiaryRepositoryPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                            try (InputStream is = Files.newInputStream(srcDockerSubsidiaryRepositoryPath)) {
                                //同步附属文件
                                artifactManagementService.store(targetDockerSubsidiaryRepositoryPath, is);
                            } catch (IOException e) {
                                log.error("Do copy srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}] error [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
        }
    }

    public void handleCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) throws Exception {
        final String srcStorageId = srcRepository.getStorage().getId(),
                srcRepositoryId = srcRepository.getId(),
                targetStorageId = targetRepository.getStorage().getId(),
                targetRepositoryId = targetRepository.getId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(srcRepository.getLayout(), sourcePath);
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
        for (RepositoryPath srcRepositoryPath : list) {

            RepositoryPath path = srcRepositoryPath;
            if (targetPath != null) {
                String filePath = path.getPath().replace(sourcePath.getPath(), targetPath.getPath());
                path = repositoryPathResolver.resolve(targetRepository, filePath);
            }

            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(path));
            if (!RepositoryFiles.isArtifact(srcRepositoryPath)) {
                log.info(String.format("RepositoryPath：%s not is artifact skip", srcRepositoryPath));
                continue;
            }
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            RepositoryPath targetBlobPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcBlobPath));
                            if (Files.exists(targetBlobPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcBlobPath.toString(), targetBlobPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcBlobPath, targetBlobPath);
                            try (InputStream inputStream = Files.newInputStream(srcBlobPath)) {
                                artifactManagementService.store(targetBlobPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcBlobPath, targetBlobPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            RepositoryPath targetManiFestPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcMainFestPath));
                            if (Files.exists(targetManiFestPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcMainFestPath.toString(), targetManiFestPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcMainFestPath, targetManiFestPath);
                            try (InputStream inputStream = Files.newInputStream(srcMainFestPath)) {
                                artifactManagementService.store(targetManiFestPath, inputStream);
                            } catch (Exception e) {
                                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcMainFestPath, targetManiFestPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
            log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcRepositoryPath, targetRepositoryPath);
            try (InputStream is = Files.newInputStream(srcRepositoryPath)) {
                //同步metadata
                setMetaData(targetRepositoryPath, getMetaData(srcRepositoryPath));
                artifactManagementService.store(targetRepositoryPath, is);
            } catch (IOException e) {
                log.error("Do copy srcRepositoryPath [{}] targetManiFestPath [{}] error [{}]", srcRepositoryPath, targetRepositoryPath, ExceptionUtils.getStackTrace(e));
                throw new Exception(e.getMessage());
            }
            handleMaven(targetRepositoryPath);
            if (isDocker) {
                List<DockerSubsidiary> dockerSubsidiaries = DockerUtils.getDockerSubsidiaryFilePaths(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(dockerSubsidiaries)) {
                    RepositoryPath srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath = null;
                    for (DockerSubsidiary dockerSubsidiary : dockerSubsidiaries) {
                        srcDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, dockerSubsidiary.getPath());
                        if (Files.exists(srcDockerSubsidiaryRepositoryPath)) {
                            targetDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, dockerSubsidiary.getPath());
                            if (Files.exists(targetDockerSubsidiaryRepositoryPath) && RepositoryFiles.validateChecksum(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath)) {
                                log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcDockerSubsidiaryRepositoryPath.toString(), targetDockerSubsidiaryRepositoryPath.toString());
                                continue;
                            }
                            log.info("Do copy srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                            try (InputStream is = Files.newInputStream(srcDockerSubsidiaryRepositoryPath)) {
                                //同步附属文件
                                artifactManagementService.store(targetDockerSubsidiaryRepositoryPath, is);
                            } catch (IOException e) {
                                log.error("Do copy srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}] error [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath, ExceptionUtils.getStackTrace(e));
                                throw new Exception(e.getMessage());
                            }
                        }
                    }
                }
            }
        }
    }

    public void handleFastCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) throws Exception {
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(srcRepository.getLayout(), sourcePath);
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask;
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
        for (RepositoryPath srcRepositoryPath : list) {
            futureTask = new FutureTask<String>(() -> {
                repositoryPathCopy(sourcePath, srcRepository, targetPath, targetRepository, srcRepositoryPath, isDocker);
                return "";
            });
            futureTaskList.add(futureTask);
            asyncCopyThreadPoolTaskExecutor.submit(futureTask);
        }
        for (FutureTask<String> task : futureTaskList) {
            task.get();
        }
        handleRpm(targetRepository);
        handleDebian(targetPath);
    }

    public void copyFile(RepositoryPath sourcePath, RepositoryPath targetPath) throws IOException {
        String copyTypeKey = "COPY_TYPE_KEY";
        String copyType = distributedCacheComponent.get(copyTypeKey);
        boolean exist = Files.exists(targetPath);
        Files.createDirectories(targetPath.getParent());
        long startTime = System.currentTimeMillis();
        if (StringUtils.isBlank(copyType) || "1".equals(copyType)) {
            Files.copy(sourcePath.getTarget(), targetPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
        } else {
            copyFile2(sourcePath, targetPath, copyType);
        }
        log.info("Copy file [{}] take time [{}] ms", sourcePath, System.currentTimeMillis() - startTime);
        if (RepositoryFiles.isArtifact(targetPath)) {
            //生成图库索引
            artifactService.copyArtifact(sourcePath, targetPath);
            //复制checksum文件
            sourcePath.getFileSystem().provider().resolveChecksumPathMap(sourcePath).forEach((key, value) -> {
                if (Files.exists(value)) {
                    try {
                        RepositoryPath checksumPath = targetPath.getParent().resolve(FilenameUtils.getName(value.toString()));
                        Files.copy(value.getTarget(), checksumPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
                    } catch (FileAlreadyExistsException e) {
                        //destination file already exists
                    } catch (Exception ex) {
                        log.warn("Copy checksumPath [{}] [{}] [{}] error [{}]", sourcePath.getStorageId(), sourcePath.getRepositoryId(), sourcePath.toString(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            });
            Artifact artifact = targetPath.getArtifactEntry();
            if (Objects.nonNull(artifact) && StringUtils.isNotBlank(artifact.getMetadata())) {
                //缓存元数据文件
                artifactComponent.cacheArtifactMetadata(targetPath, artifact.getMetadata());
            }
            if (exist) {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(targetPath);
            } else {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(targetPath);
            }
        } else if (RepositoryFiles.isMetadata(targetPath)) {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(targetPath);
        }
    }

    public void copyFile2(RepositoryPath sourcePath, RepositoryPath targetPath, String copyType) throws IOException {
        if (StringUtils.isBlank(copyType) || "1".equals(copyType)) {
            return;
        }
        Files.createDirectories(targetPath.getParent());
        Files.createFile(targetPath);
        try (FileChannel fileChannel = new FileInputStream(sourcePath.toString()).getChannel();
             FileChannel destChannel = new FileOutputStream(targetPath.toString()).getChannel()) {
            long fileSize = fileChannel.size();
            if ("2".equals(copyType)) {
                for (long left = fileSize; left > 0; ) {
                    left -= fileChannel.transferTo((fileSize - left), left, destChannel);
                }
            } else if ("3".equals(copyType)) {
                long position = 0;
                while (position < fileSize) {
                    long chunkSize = Math.min(BUFFER_SIZE, fileSize - position);
                    MappedByteBuffer buffer = fileChannel.map(FileChannel.MapMode.READ_ONLY, position, chunkSize);
                    destChannel.write(buffer);
                    position += chunkSize;
                }
            }
        }
    }

    private void repositoryPathCopy(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository, RepositoryPath srcRepositoryPath, boolean isDocker) throws IOException {
        final String srcStorageId = srcRepository.getStorage().getId(),
                srcRepositoryId = srcRepository.getId(),
                targetStorageId = targetRepository.getStorage().getId(),
                targetRepositoryId = targetRepository.getId();
        RepositoryPath path = srcRepositoryPath;
        if (targetPath != null) {
            String filePath = path.getPath().replace(sourcePath.getPath(), targetPath.getPath());
            path = repositoryPathResolver.resolve(targetRepository, filePath);
        }

        RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(path));
        if (!RepositoryFiles.isArtifact(srcRepositoryPath)) {
            log.info(String.format("RepositoryPath：%s not is artifact skip", srcRepositoryPath));
            return;
        }
        if (isDocker) {
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest manifest : imageManifestList) {
                    List<String> layerList = getAllLayerList(manifest);
                    //blobs
                    for (String layer : layerList) {
                        RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                        RepositoryPath targetBlobPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcBlobPath));
                        if (Files.exists(targetBlobPath)) {
                            log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcBlobPath.toString(), targetBlobPath.toString());
                            continue;
                        }
                        log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcBlobPath, targetBlobPath);
                        copyFile(srcBlobPath, targetBlobPath);
                    }
                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                        RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                        RepositoryPath targetManiFestPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcMainFestPath));
                        if (Files.exists(targetManiFestPath)) {
                            log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcMainFestPath.toString(), targetManiFestPath.toString());
                            continue;
                        }
                        log.info("Do copy srcRepositoryPath [{}] targetManiFestPath [{}]", srcMainFestPath, targetManiFestPath);
                        copyFile(srcMainFestPath, targetManiFestPath);
                    }
                }
            }
        }
        log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}]", srcRepositoryPath, targetRepositoryPath);
        long startTime = System.currentTimeMillis();
        copyFile(srcRepositoryPath, targetRepositoryPath);
        handleMaven(targetRepositoryPath);
        log.info("Copy repositoryPath [{}] take time [{}] ms", srcRepositoryPath, System.currentTimeMillis() - startTime);
        if (isDocker) {
            List<DockerSubsidiary> dockerSubsidiaries = DockerUtils.getDockerSubsidiaryFilePaths(srcRepositoryPath);
            if (CollectionUtils.isNotEmpty(dockerSubsidiaries)) {
                RepositoryPath srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath = null;
                for (DockerSubsidiary dockerSubsidiary : dockerSubsidiaries) {
                    srcDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, dockerSubsidiary.getPath());
                    if (Files.exists(srcDockerSubsidiaryRepositoryPath)) {
                        targetDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, dockerSubsidiary.getPath());
                        if (Files.exists(targetDockerSubsidiaryRepositoryPath) && RepositoryFiles.validateChecksum(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath)) {
                            log.info("Do copy srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcDockerSubsidiaryRepositoryPath.toString(), targetDockerSubsidiaryRepositoryPath.toString());
                            continue;
                        }
                        log.info("Do copy srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                        copyFile(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                    }
                }
            }
        }
    }

    private void handleMaven(RepositoryPath targetRepositoryPath) {
        try {
            if (!ProductTypeEnum.Maven.getFoLibraryName().equals(targetRepositoryPath.getRepository().getLayout())) {
                return;
            }
            artifactMetadataService.rebuildMetadata(targetRepositoryPath.getStorageId(), targetRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(targetRepositoryPath));
        } catch (Exception ex) {
            log.error("Rebuild maven metadata path [{}] error [{}]", targetRepositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    private void handleRpm(Repository repository) {
        try {
            if (!ProductTypeEnum.Rpm.getFoLibraryName().equals(repository.getLayout())) {
                return;
            }
            RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
            rpmRepoIndexer.indexWriter(repository);
        } catch (Exception ex) {
            log.error("Rebuild rpm index storage [{}] repository [{}] error [{}]", repository.getStorage().getId(), repository.getId(), ExceptionUtils.getStackTrace(ex));
        }

    }

    private void handleDebian(RepositoryPath repositoryPath) {
        try {
            if (!ProductTypeEnum.Debian.getFoLibraryName().equals(repositoryPath.getRepository().getLayout())) {
                return;
            }
            String distribution = repositoryPath.getExtAttribute().get(DebianConstant.ATTR_DISTRIBUTION);
            String component = repositoryPath.getExtAttribute().get(DebianConstant.ATTR_COMPONENT);
            String architecture = repositoryPath.getExtAttribute().get(DebianConstant.ATTR_ARCHITECTURE);
            DebianCoordinates coordinate = new DebianCoordinates();
            coordinate.setArchitecture(architecture);
            coordinate.setDistribution(distribution);
            coordinate.setComponent(component);
            DebianIndexEvent addEvent = DebianUtils.generateEvent(coordinate, repositoryPath.getArtifactEntry(), DeltaIndexEventType.ADD);
            DebianIncrementalIndexer debianIncrementalIndexer = (DebianIncrementalIndexer) SpringContextUtil.getBean("debianIncrementalIndexer");
            debianIncrementalIndexer.index(repositoryPath.getRepository(), Sets.newSet(addEvent));
            DebianReleaseMetadataIndexer debianIndexer = new DebianReleaseMetadataIndexer(repositoryPath.getRepository(), Collections.emptyList(), repositoryPathResolver);
            debianIndexer.indexRelease(distribution);
        } catch (Exception ex) {
            log.error("Rebuild debian index storage [{}] repository [{}] error [{}]", repositoryPath.getRepository().getStorage().getId(), repositoryPath.getRepositoryId(), ExceptionUtils.getStackTrace(ex));
        }

    }

    public void handleFastMove(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository) throws Exception {
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(srcRepository.getLayout(), sourcePath);
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask;
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
        for (RepositoryPath srcRepositoryPath : list) {
            futureTask = new FutureTask<String>(() -> {
                repositoryPathMove(sourcePath, srcRepository, targetPath, targetRepository, srcRepositoryPath, isDocker);
                return "";
            });
            futureTaskList.add(futureTask);
            asyncCopyThreadPoolTaskExecutor.submit(futureTask);
        }
        for (FutureTask<String> task : futureTaskList) {
            task.get();
        }
        handleRpm(targetRepository);
        handleDebian(targetPath);
    }

    private void repositoryPathMove(RepositoryPath sourcePath, Repository srcRepository, RepositoryPath targetPath, Repository targetRepository, RepositoryPath srcRepositoryPath, boolean isDocker) throws IOException {
        final String srcStorageId = srcRepository.getStorage().getId(),
                srcRepositoryId = srcRepository.getId(),
                targetStorageId = targetRepository.getStorage().getId(),
                targetRepositoryId = targetRepository.getId();
        RepositoryPath path = srcRepositoryPath;
        if (targetPath != null) {
            String filePath = path.getPath().replace(sourcePath.getPath(), targetPath.getPath());
            path = repositoryPathResolver.resolve(targetRepository, filePath);
        }

        RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(path));
        if (!RepositoryFiles.isArtifact(srcRepositoryPath)) {
            log.info(String.format("RepositoryPath：%s not is artifact skip", srcRepositoryPath));
            return;
        }
        if (isDocker) {
            List<DockerSubsidiary> dockerSubsidiaries = DockerUtils.getDockerSubsidiaryFilePaths(srcRepositoryPath);
            if (CollectionUtils.isNotEmpty(dockerSubsidiaries)) {
                RepositoryPath srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath = null;
                for (DockerSubsidiary dockerSubsidiary : dockerSubsidiaries) {
                    srcDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, dockerSubsidiary.getPath());
                    if (Files.exists(srcDockerSubsidiaryRepositoryPath)) {
                        targetDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, dockerSubsidiary.getPath());
                        if (Files.exists(targetDockerSubsidiaryRepositoryPath) && RepositoryFiles.validateChecksum(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath)) {
                            log.info("Do move srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcDockerSubsidiaryRepositoryPath.toString(), targetDockerSubsidiaryRepositoryPath.toString());
                            continue;
                        }
                        log.info("Do move srcRepositoryPath [{}] targetDockerSubsidiaryRepositoryPath [{}]", srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                        copyFile(srcDockerSubsidiaryRepositoryPath, targetDockerSubsidiaryRepositoryPath);
                    }
                }
            }
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest manifest : imageManifestList) {
                    List<String> layerList = getAllLayerList(manifest);
                    //blobs
                    for (String layer : layerList) {
                        RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                        RepositoryPath targetBlobPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcBlobPath));
                        if (Files.exists(targetBlobPath)) {
                            log.info("Do move srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcBlobPath.toString(), targetBlobPath.toString());
                            continue;
                        }
                        log.info("Do move srcRepositoryPath [{}] targetManiFestPath [{}]", srcBlobPath, targetBlobPath);
                        copyFile(srcBlobPath, targetBlobPath);
                    }
                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                        RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                        RepositoryPath targetManiFestPath = repositoryPathResolver.resolve(targetStorageId, targetRepositoryId, RepositoryFiles.relativizePath(srcMainFestPath));
                        if (Files.exists(targetManiFestPath)) {
                            log.info("Do move srcRepositoryPath [{}] targetRepositoryPath [{}] exists skip...", srcMainFestPath.toString(), targetManiFestPath.toString());
                            continue;
                        }
                        log.info("Do move srcRepositoryPath [{}] targetManiFestPath [{}]", srcMainFestPath, targetManiFestPath);
                        copyFile(srcMainFestPath, targetManiFestPath);
                    }
                }
            }
        }
        log.info("Do move srcRepositoryPath [{}] targetRepositoryPath [{}]", srcRepositoryPath, targetRepositoryPath);
        if (isDocker) {
            copyFile(srcRepositoryPath, targetRepositoryPath);
            RepositoryFiles.delete(srcRepositoryPath, true);
        } else {
            moveFile(srcRepositoryPath, targetRepositoryPath);
        }
        handleMaven(targetRepositoryPath);
    }

    public void moveFile(RepositoryPath sourcePath, RepositoryPath targetPath) throws IOException {
        boolean exist = Files.exists(targetPath);
        Files.createDirectories(targetPath.getParent());
        long startTime = System.currentTimeMillis();
        Files.move(sourcePath.getTarget(), targetPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
        log.info("Move file [{}] take time [{}] ms", sourcePath, System.currentTimeMillis() - startTime);
        if (RepositoryFiles.isArtifact(targetPath)) {
            //生成图库索引
            artifactService.copyArtifact(sourcePath, targetPath);
            //复制checksum文件
            sourcePath.getFileSystem().provider().resolveChecksumPathMap(sourcePath).forEach((key, value) -> {
                if (Files.exists(value)) {
                    try {
                        RepositoryPath checksumPath = targetPath.getParent().resolve(FilenameUtils.getName(value.toString()));
                        Files.move(value.getTarget(), checksumPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
                    } catch (FileAlreadyExistsException e) {
                        //destination file already exists
                    } catch (Exception ex) {
                        log.warn("Copy checksumPath [{}] [{}] [{}] error [{}]", sourcePath.getStorageId(), sourcePath.getRepositoryId(), sourcePath.toString(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            });
            Artifact artifact = targetPath.getArtifactEntry();
            if (Objects.nonNull(artifact) && StringUtils.isNotBlank(artifact.getMetadata())) {
                RepositoryPath sourceArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(sourcePath);
                if (Files.exists(sourceArtifactMetadataPath)) {
                    //移动元数据文件
                    RepositoryPath targetArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(targetPath);
                    Files.createDirectories(targetArtifactMetadataPath.getParent());
                    Files.move(sourceArtifactMetadataPath.getTarget(), targetArtifactMetadataPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
                } else {
                    //缓存元数据文件
                    artifactComponent.cacheArtifactMetadata(targetPath, artifact.getMetadata());
                }
            }
            if (exist) {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(targetPath);
            } else {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(targetPath);
            }
        } else if (RepositoryFiles.isMetadata(targetPath)) {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(targetPath);
        }
        RepositoryFiles.delete(sourcePath, true);
    }

    public PromotionFileRelativePath getFileRelativePaths(RepositoryPath repositoryPath) throws Exception {
        Map<String, Object> metaData = Maps.newHashMap();
        String layout = repositoryPath.getRepository().getLayout(), srcStorageId = repositoryPath.getStorageId(), srcRepositoryId = repositoryPath.getRepositoryId();
        List<RepositoryPath> list = RepositoryPathUtil.getPaths(layout, repositoryPath);
        List<String> repositoryPaths = Lists.newArrayList();
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout);
        for (RepositoryPath srcRepositoryPath : list) {
            if (isDocker) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        List<String> layerList = getAllLayerList(manifest);
                        //blobs
                        for (String layer : layerList) {
                            RepositoryPath srcBlobPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.BLOBS + File.separator + layer);
                            log.info("Pull find blob srcRepositoryPath [{}]", srcBlobPath);
                            repositoryPaths.add(RepositoryFiles.relativizePath(srcBlobPath));
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                            log.info("Pull find manifest srcRepositoryPath [{}]", srcMainFestPath);
                            repositoryPaths.add(RepositoryFiles.relativizePath(srcMainFestPath));
                        }
                    }
                }
            }
            log.info("Pull find srcRepositoryPath [{}]", srcRepositoryPath);
            repositoryPaths.add(RepositoryFiles.relativizePath(srcRepositoryPath));
            metaData.put(RepositoryFiles.relativizePath(srcRepositoryPath), getMetaData(srcRepositoryPath));
            if (isDocker) {
                List<DockerSubsidiary> dockerSubsidiaries = DockerUtils.getDockerSubsidiaryFilePaths(srcRepositoryPath);
                if (CollectionUtils.isNotEmpty(dockerSubsidiaries)) {
                    RepositoryPath srcDockerSubsidiaryRepositoryPath;
                    for (DockerSubsidiary dockerSubsidiary : dockerSubsidiaries) {
                        srcDockerSubsidiaryRepositoryPath = repositoryPathResolver.resolve(srcStorageId, srcRepositoryId, dockerSubsidiary.getPath());
                        if (Files.exists(srcDockerSubsidiaryRepositoryPath)) {
                            log.info("Pull find subsidiary srcRepositoryPath [{}]", srcDockerSubsidiaryRepositoryPath);
                            repositoryPaths.add(RepositoryFiles.relativizePath(srcDockerSubsidiaryRepositoryPath));
                        }
                    }
                }
            }
        }
        return new PromotionFileRelativePath(repositoryPaths, metaData);
    }

    public String getMetaData(RepositoryPath srcPath) {
        String rs = "";
        try {
            if (Objects.isNull(srcPath) || !Files.exists(srcPath)) {
                return rs;
            }
            Artifact artifact = artifactWebService.getArtifact(srcPath);
            if (Objects.isNull(artifact)) {
                return rs;
            }
            rs = artifact.getMetadata();
        } catch (Exception e) {
            log.error("Exception {}", ExceptionUtils.getStackTrace(e));
        }
        return rs;
    }

    /**
     * 处理metadata
     *
     * @param repositoryPath repositoryPath
     * @param metadata       metadata
     */
    public void setMetaData(RepositoryPath repositoryPath, String metadata) {
        if (Objects.nonNull(repositoryPath) && StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
            try {
                if (!RepositoryFiles.isArtifact(repositoryPath)) {
                    return;
                }
                Artifact artifact = Optional.ofNullable(repositoryPath.getArtifactEntry())
                        .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                                RepositoryFiles.readCoordinates(repositoryPath)));
                artifact.setMetadata(metadata);
                repositoryPath.setArtifact(artifact);
            } catch (Exception ex) {
                log.error("setMetaData Exception {} repositoryPath {} metadata {}", ExceptionUtils.getStackTrace(ex), repositoryPath.toString(), metadata);
            }
        }
    }

    public List<String> getAllLayerList(ImageManifest imageManifest) {
        if (Objects.nonNull(imageManifest) && CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
            List<String> layerList = imageManifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            if (Objects.nonNull(imageManifest.getConfig())) {
                layerList.add(imageManifest.getConfig().getDigest());
            }
            return layerList;
        }
        return Collections.emptyList();
    }

    private Integer getChunkIndex(String chunk) {
        String regex = "chunk(\\d+)";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(chunk);

        if (matcher.matches()) {
            String chunkNumber = matcher.group(1);
            return Integer.parseInt(chunkNumber);
        }
        throw new RuntimeException("chunk is not a number");
    }

    private List<ArtifactSliceUploadHttpEntityBuilder> getArtifactSliceUploadHttpEntityList(String storageId, String repositoryId, String saveUri, RepositoryPath sourceRepositoryPath, long chunkSize) {
        try {
            final long fileLength = Files.size(sourceRepositoryPath);
            final int threadCount = BigDecimal.valueOf(fileLength).divide(BigDecimal.valueOf(chunkSize), 0, RoundingMode.CEILING).intValue();
            //final String md5 = FileUtils.getMD5(Files.newInputStream(artifactPath));
            final String sourceStorageId = sourceRepositoryPath.getStorageId(), sourceRepositoryId = sourceRepositoryPath.getRepositoryId(), sourceArtifactPath = RepositoryFiles.relativizePath(sourceRepositoryPath);
            log.info("Calculate the file [{}] [{}] [{}] file size [{}]", sourceStorageId, sourceRepositoryId, sourceArtifactPath, fileLength);
            long begin = System.currentTimeMillis();
            LayoutFileSystemProvider provider = (LayoutFileSystemProvider) sourceRepositoryPath.getFileSystem().provider();
            final RepositoryPath checksumPath = provider.getChecksumPath(sourceRepositoryPath, MessageDigestAlgorithms.MD5);
            String md5 = "";
            if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                md5 = Files.readString(checksumPath);
            }
            if (StringUtils.isBlank(md5)) {
                md5 = MessageDigestUtils.calculateChecksum(sourceRepositoryPath, MessageDigestAlgorithms.MD5);
                //md5 = FileUtils.getMD5(Files.newInputStream(sourceRepositoryPath));
            }

            log.info("Calculated the file [{}] [{}] [{}] md5 is [{}] file size [{}] time consuming [{}] ms", sourceStorageId, sourceRepositoryId, sourceArtifactPath, md5, fileLength, System.currentTimeMillis() - begin);
            final String mergeId = UUID.randomUUID().toString(true);

            String finalMd5 = md5;
            return IntStream.range(0, threadCount).mapToObj(index -> {
                long startLength = index * chunkSize;
                long currentChunkSize = chunkSize;
                // 如果是最后一个切片，调整其大小
                if (index == threadCount - 1) {
                    long remainingSize = fileLength - startLength;
                    currentChunkSize = Math.min(chunkSize, remainingSize);
                }
                try {
                    return new ArtifactSliceUploadHttpEntityBuilder()
                            .setStorageId(storageId)
                            .setRepositoryId(repositoryId)
                            .setPath(saveUri)
                            .setMergeId(mergeId)
                            .setChunkIndex(index + 1)
                            .setChunkIndexMax(threadCount)
                            .setOriginFileMd5(finalMd5)
                            .setArtifactPath(sourceRepositoryPath)
                            .setStartLength(startLength)
                            .setChunkSize(currentChunkSize);
                } catch (Exception e) {
                    log.error("构建文件切片请求对象失败", e);
                    throw new RuntimeException(e);
                }
            }).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("构建文件切片请求集合失败", e);
            throw new RuntimeException(e);
        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityBuilder {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private String storageId;
        private String repositoryId;
        private String path;
        private String mergeId;
        private Integer chunkIndex;
        private Integer chunkIndexMax;
        private String originFileMd5;
        private Path artifactPath;
        private Long startLength;
        private Long chunkSize;

        public HttpEntity build() {
            try {
                return MultipartEntityBuilder.create()
                        .setContentType(ContentType.MULTIPART_FORM_DATA)
                        .addPart("storageId", new StringBody(storageId))
                        .addPart("repositoryId", new StringBody(repositoryId))
                        .addPart("path", new StringBody(path))
                        .addPart("mergeId", new StringBody(mergeId))
                        .addPart("chunkIndex", new StringBody(String.valueOf(chunkIndex)))
                        .addPart("chunkIndexMax", new StringBody(String.valueOf(chunkIndexMax)))
                        .addPart("originFileMd5", new StringBody(originFileMd5))
                        .addPart("file", new InputStreamBody(new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize), "chunk" + chunkIndex))
                        .build();
            } catch (Exception e) {
                log.error("构建文件切片HttpEntity请求失败", e);
                return null;
            }
        }

        public ArtifactSliceUploadReq buildV3() {
            ArtifactSliceUploadReq artifactSliceUploadReq = new ArtifactSliceUploadReq();
            artifactSliceUploadReq.setStorageId(storageId);
            artifactSliceUploadReq.setRepositoryId(repositoryId);
            artifactSliceUploadReq.setPath(path);
            artifactSliceUploadReq.setMergeId(mergeId);
            artifactSliceUploadReq.setChunkIndex(chunkIndex);
            artifactSliceUploadReq.setChunkIndexMax(chunkIndexMax);
            artifactSliceUploadReq.setOriginFileMd5(originFileMd5);
            // 从文件系统中读取文件
            String name = "file"; // 表单字段名
            String originalFileName = "chunk" + chunkIndex;
            String contentType = "application/octet-stream"; // 文件的内容类型
            byte[] content = new byte[0];
            try (BufferedInputStreamWrapper bufferedInputStreamWrapper = new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize)) {
                content = bufferedInputStreamWrapper.readAllBytes();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            MultipartFile multipartFile = new MockMultipartFile(name, originalFileName, contentType, content);
            artifactSliceUploadReq.setFile(multipartFile);
            return artifactSliceUploadReq;
        }

        public HashMap<String, Object> buildV2() {
            HashMap<String, Object> map = new HashMap<>();
            map.put("storageId", storageId);
            map.put("repositoryId", repositoryId);
            map.put("path", path);
            map.put("mergeId", mergeId);
            map.put("chunkIndex", String.valueOf(chunkIndex));
            map.put("chunkIndexMax", String.valueOf(chunkIndexMax));
            map.put("originFileMd5", originFileMd5);
            BufferedInputStreamWrapper bufferedInputStreamWrapper = null;
            try {
                bufferedInputStreamWrapper = new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            try {
                byte[] bytes = bufferedInputStreamWrapper.readAllBytes();
                map.put("file", bytes);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            return map;
        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityBuilderV2 {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private String storageId;
        private String repositoryId;
        private String path;
        private String mergeId;
        private Integer chunkIndex;
        private Integer chunkIndexMax;
        private String originFileMd5;
        private Path artifactPath;
        private Long startLength;
        private Long chunkSize;

        public HashMap<String, Object> buildV2() {
            HashMap<String, Object> map = new HashMap<>();
            map.put("storageId", storageId);
            map.put("repositoryId", repositoryId);
            map.put("path", path);
            map.put("mergeId", mergeId);
            map.put("chunkIndex", String.valueOf(chunkIndex));
            map.put("chunkIndexMax", String.valueOf(chunkIndexMax));
            map.put("originFileMd5", originFileMd5);
            try {
                map.put("file", new InputStreamBody(new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize), "chunk" + chunkIndex));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            return map;
        }
    }

    public RepositoryPath getTargetPath(ArtifactPromotion artifactPromotion, RepositoryPath srcPath, Repository destRepository) {
        if (ProductTypeEnum.Debian.getFoLibraryName().equals(srcPath.getRepository().getLayout())) {
            try {
                Map<String, String> coordinates = srcPath.getArtifactEntry().getArtifactCoordinates().getCoordinates();
                String arrtString = DebianUtils.getArrtString(coordinates.get(DebianConstant.DISTRIBUTION), coordinates.get(DebianConstant.COMPONENT), coordinates.get(DebianConstant.ARCHITECTURE));
                String target = artifactPromotion.getTargetPath() + ";" + arrtString;
                return artifactPromotion.getTargetPath() == null ? null : repositoryPathResolver.resolve(destRepository, target);
            } catch (IOException e) {
                throw new IllegalArgumentException("The source path does not exist!");
            }
        } else {
            return artifactPromotion.getTargetPath() == null ? null : repositoryPathResolver.resolve(destRepository, artifactPromotion.getTargetPath());

        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityResponse {
        /**
         * 制品切片记录ID
         */
        private Long chunkArtifactRecordId;
        private Boolean success;
        private String failedReason;
    }
}
