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
package com.folib.components.webhook;

import com.alibaba.fastjson.JSON;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.layout.DockerComponent;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.domain.migrate.ArtifactMigrateInfo;
import com.folib.entity.Dict;
import com.folib.entity.WebhookEventsLog;
import com.folib.enums.WebhookEventsStatusEnum;
import com.folib.enums.WebhookEventsTypeEnum;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.schema2.ContainerConfigurationManifest;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.DirectoryListingService;
import com.folib.services.WebhookEventsLogService;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.utils.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.RepositoryHandle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2025/3/7
 **/
@Slf4j
@Component
public class DockerWebhooksEventProvider extends BaseWebhookEventsProvider {

    @Autowired
    private WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private DockerComponent dockerComponent;

    @Autowired
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Autowired
    public DockerWebhooksEventProvider(RepositoryPathResolver repositoryPathResolver, ArtifactResolutionService artifactResolutionService, ArtifactManagementService artifactManagementService, SecurityUtils securityUtils, WebhookEventsLogService webhookEventsLogService, PromotionUtil promotionUtil) {
        super(repositoryPathResolver, artifactResolutionService, artifactManagementService, securityUtils, webhookEventsLogService, promotionUtil);
    }

    @Override
    @PostConstruct
    public void register() {
        webhookEventsProviderRegistry.addProvider(WebhookEventsTypeEnum.DOCKER.getType(), this);
        log.info("Registered webhook events '{}' with alias '{}'.",
                getClass().getCanonicalName(), WebhookEventsTypeEnum.DOCKER.getType());
    }

    private boolean localRepository(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = true;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(), failureReason = "";
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        ArtifactMigrateInfo jfrogInfo = JSON.parseObject(artifactMigrateInfo.getAlias(), ArtifactMigrateInfo.class);
        String tag = artifactData.getTag();
        if (StringUtils.isBlank(tag)) {
            String[] arr = artifactData.getPath().split("/");
            tag = arr[arr.length - 2];
        }
        // 获取制品
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(jfrogInfo.getRemotePreUrl()).setUsername(jfrogInfo.getUsername()).setPassword(jfrogInfo.getPassword()).build()) {
            // 设置admin权限
            securityUtils.setAdminAuthentication();
            RepositoryHandle repository = artifactory.repository(webhook.getData().getRepoKey());
            // 1.下载 manifest.json
            String digestOrTag = "";
            boolean isTag = false;
            if (!tag.startsWith(GlobalConstants.SHA_256)) {
                digestOrTag = tag;
                isTag = true;
            } else {
                digestOrTag = tag.replace("__", ":");
            }
            String targetPath = "";
            String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(tag)), GlobalConstants.SEPARATOR);
            if (isTag) {
                targetPath = imagePath + GlobalConstants.SEPARATOR + digestOrTag;
            } else {
                targetPath = imagePath + GlobalConstants.SEPARATOR + ".temp_" + digestOrTag;
            }
            String shaChecksum = "sha256:" + artifactData.getSha256();
            String tagManifestRepoPath = String.format("%s/%s", targetPath, shaChecksum);
            RepositoryPath tagManifestRepositoryPath = rootRepositoryPath.resolve(tagManifestRepoPath);
            String tempManifestRepoPath = String.format("%s/%s", targetPath, "manifest.json");
            try (InputStream artifactStream = repository.download(tempManifestRepoPath).doDownload()) {
                artifactManagementService.store(tagManifestRepositoryPath, artifactStream);
                if (!Files.exists(tagManifestRepositoryPath)) {
                    result = false;
                }
            }
            RepositoryPath manifestRepoPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST).resolve(shaChecksum);
            manifestRepoPath.setArtifactPath(imagePath);
            try (InputStream inputStream = Files.newInputStream(tagManifestRepositoryPath)) {
                artifactManagementService.store(manifestRepoPath, inputStream);
            }
            if (!Files.exists(manifestRepoPath)) {
                result = false;
            }
            // 2.下载blobs
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(manifestRepoPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest imageManifest : imageManifestList) {
                    ContainerConfigurationManifest containerConfigurationManifest = imageManifest.getConfig();
                    if (Objects.nonNull(containerConfigurationManifest) && StringUtils.isNotBlank(containerConfigurationManifest.getDigest())) {
                        RepositoryPath blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + containerConfigurationManifest.getDigest());
                        String digestPath = targetPath + File.separator + containerConfigurationManifest.getDigest().replace(":", "__");
                        try (InputStream blobsInputStream = repository.download(digestPath).doDownload()) {
                            artifactManagementService.store(blobsRepositoryPath, blobsInputStream);
                        }
                        if (!Files.exists(blobsRepositoryPath)) {
                            result = false;
                        }
                    }
                    if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                        for (LayerManifest layerManifest : imageManifest.getLayers()) {
                            if (StringUtils.isNotBlank(layerManifest.getDigest())) {
                                RepositoryPath blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + layerManifest.getDigest());
                                blobsRepositoryPath.setArtifactPath(imagePath);
                                String digestPath = targetPath + File.separator + layerManifest.getDigest().replace(":", "__");
                                try (InputStream blobsInputStream = repository.download(digestPath).doDownload()) {
                                    artifactManagementService.store(blobsRepositoryPath, blobsInputStream);
                                }
                                if (!Files.exists(blobsRepositoryPath)) {
                                    result = false;
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            result = false;
            failureReason = ex.getMessage();
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
        } finally {
            securityUtils.clearAuthentication();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return true;
    }

    private boolean proxyRepository(WebhookDto webhook, RepositoryPath repositoryPath, int type) {
        boolean result = true;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(), failureReason = "";
        try {
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            String tag = artifactData.getTag();
            if (StringUtils.isBlank(tag)) {
                String[] arr = artifactData.getPath().split("/");
                tag = arr[arr.length - 2];
            }
            String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(tag)), GlobalConstants.SEPARATOR);
            RemoteRepository remoteRepository = repositoryPath.getRepository().getRemoteRepository();
            String remoteUrl = StringUtils.removeEnd(remoteRepository.getUrl(), GlobalConstants.SEPARATOR);
            String digestOrTag = "";
            boolean isTag = false;
            if (!tag.startsWith(GlobalConstants.SHA_256)) {
                digestOrTag = tag;
                isTag = true;
            } else {
                digestOrTag = tag.replace("__", ":");
            }
            if (!remoteUrl.endsWith(GlobalConstants.DOCKER_V2) || imagePath.split(GlobalConstants.SEPARATOR).length > 1) {
                imagePath = imagePath.replace(GlobalConstants.DOCKER_DEFAULT_REPO.concat(GlobalConstants.SEPARATOR), "");
            }
            RepositoryPath manifestRepositoryPath = dockerComponent.resolveManifest(storageId, repositoryId, imagePath, digestOrTag);
            if (Objects.isNull(manifestRepositoryPath)) {
                result = false;
            }
            if (isTag && Objects.nonNull(manifestRepositoryPath)) {
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(manifestRepositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    RepositoryPath blobsRepositoryPath;
                    for (ImageManifest imageManifest : imageManifestList) {
                        ContainerConfigurationManifest containerConfigurationManifest = imageManifest.getConfig();
                        if (Objects.nonNull(containerConfigurationManifest) && StringUtils.isNotBlank(containerConfigurationManifest.getDigest())) {
                            blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + containerConfigurationManifest.getDigest());
                            String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(imagePath, "/"), containerConfigurationManifest.getDigest());
                            blobsRepositoryPath.setTargetUrl(targetUrl);
                            blobsRepositoryPath.setArtifactPath(imagePath);
                            artifactResolutionService.resolvePath(blobsRepositoryPath);
                            if (!Files.exists(blobsRepositoryPath)) {
                                result = false;
                            }
                        }
                        if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                            for (LayerManifest layerManifest : imageManifest.getLayers()) {
                                if (StringUtils.isNotBlank(layerManifest.getDigest())) {
                                    blobsRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + layerManifest.getDigest());
                                    String targetUrl = String.format("%s/blobs/%s", StringUtils.removeEnd(imagePath, "/"), layerManifest.getDigest());
                                    blobsRepositoryPath.setTargetUrl(targetUrl);
                                    blobsRepositoryPath.setArtifactPath(imagePath);
                                    artifactResolutionService.resolvePath(blobsRepositoryPath);
                                    if (!Files.exists(blobsRepositoryPath)) {
                                        result = false;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            result = false;
            failureReason = ex.getMessage();
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return true;
    }

    @Override
    protected boolean deployedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        if (RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType())) {
            return localRepository(webhook, repositoryPath, artifactMigrateInfo, type);
        } else {
            return proxyRepository(webhook, repositoryPath, type);
        }
    }

    @Override
    protected boolean deletedEvent(WebhookDto webhook, RepositoryPath repositoryPath, int type) {
        boolean result = true;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(), failureReason = "";
        if (name.startsWith("sha256")) {
            return result;
        }
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        String tag = artifactData.getTag();
        if (StringUtils.isBlank(tag)) {
            String[] arr = artifactData.getPath().split("/");
            tag = arr[arr.length - 2];
        }
        String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(tag)), GlobalConstants.SEPARATOR);
        repositoryPath = rootRepositoryPath.resolve(imagePath).resolve(tag);
        try {
            if (Files.isDirectory(repositoryPath)) {
                securityUtils.setAdminAuthentication();
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(fileContents)) {
                    return true;
                }
                FileContent fileContent = fileContents.get(0);
                repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), fileContent.getArtifactPath());
                if (checkNotExists(repositoryPath)) {
                    return true;
                }
                if (Files.exists(repositoryPath)) {
                    if (2 == type) {
                        //定时任务触发，直接删除
                        RepositoryFiles.delete(repositoryPath);
                        result = checkNotExists(repositoryPath);
                    } else {
                        //webhook触发，保存至数据库，异步定时任务处理
                        WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                                .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.INIT.getStatus()).build();
                        webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
                        result = true;
                    }
                }
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            result = false;
            failureReason = ex.getMessage();
        } finally {
            securityUtils.clearAuthentication();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    @Override
    protected boolean movedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = true;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(),
                sourceRepoPath = artifactData.getSourceRepoPath(), targetRepoPath = artifactData.getTargetRepoPath(), targetRepositoryId = "", targetArtifactPath = "", failureReason = "";
        if (name.startsWith("sha256")) {
            return result;
        }
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        try {
            securityUtils.setAdminAuthentication();
            String tag = artifactData.getTag();
            if (StringUtils.isBlank(tag)) {
                String[] arr = artifactData.getPath().split("/");
                tag = arr[arr.length - 2];
            }
            String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(tag)), GlobalConstants.SEPARATOR);
            repositoryPath = rootRepositoryPath.resolve(imagePath).resolve(tag);
            targetRepositoryId = targetRepoPath.split("/")[0];
            targetArtifactPath = targetRepoPath.replace(targetRepositoryId + "/", "");
            String[] arr = targetArtifactPath.split("/");
            String targetTag  = arr[arr.length - 2];
            String targetImagePath = StringUtils.removeEnd(targetArtifactPath.substring(0, targetArtifactPath.indexOf(targetTag)), GlobalConstants.SEPARATOR);
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(storageId, targetRepositoryId, targetImagePath).resolve(targetTag);
            if (Files.isDirectory(repositoryPath)) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(fileContents)) {
                    return true;
                }
                FileContent fileContent = fileContents.get(0);
                repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), fileContent.getArtifactPath());
                if (checkNotExists(repositoryPath)) {
                    return true;
                }
                if (Files.exists(repositoryPath)) {
                    promotionUtil.executeFastSyncMove(repositoryPath.getParent(), repositoryPath.getRepository(), targetRepositoryPath, targetRepositoryPath.getRepository());
                }
                if (Files.exists(targetRepositoryPath)) {
                    RepositoryFiles.delete(repositoryPath.getParent());
                    return true;
                }
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            result = false;
            failureReason = ex.getMessage();
        } finally {
            securityUtils.clearAuthentication();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).sourceArtifactPath(sourceRepoPath).targetArtifactPath(targetRepoPath).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    @Override
    protected boolean copiedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = true;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(),
                sourceRepoPath = artifactData.getSourceRepoPath(), targetRepoPath = artifactData.getTargetRepoPath(), targetRepositoryId = "", targetArtifactPath = "", failureReason = "";
        if (name.startsWith("sha256")) {
            return result;
        }
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        try {
            securityUtils.setAdminAuthentication();
            String tag = artifactData.getTag();
            if (StringUtils.isBlank(tag)) {
                String[] arr = artifactData.getPath().split("/");
                tag = arr[arr.length - 2];
            }
            String imagePath = StringUtils.removeEnd(path.substring(0, path.indexOf(tag)), GlobalConstants.SEPARATOR);
            repositoryPath = rootRepositoryPath.resolve(imagePath).resolve(tag);
            targetRepositoryId = targetRepoPath.split("/")[0];
            targetArtifactPath = targetRepoPath.replace(targetRepositoryId + "/", "");
            String[] arr = targetArtifactPath.split("/");
            String targetTag  = arr[arr.length - 2];
            String targetImagePath = StringUtils.removeEnd(targetArtifactPath.substring(0, targetArtifactPath.indexOf(targetTag)), GlobalConstants.SEPARATOR);
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(storageId, targetRepositoryId, targetImagePath).resolve(targetTag);
            if (Files.isDirectory(repositoryPath)) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(fileContents)) {
                    return true;
                }
                FileContent fileContent = fileContents.get(0);
                repositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), fileContent.getArtifactPath());
                if (checkNotExists(repositoryPath)) {
                    return true;
                }
                if (Files.exists(repositoryPath)) {
                    promotionUtil.executeFastSyncCopy(repositoryPath.getParent(), repositoryPath.getRepository(), targetRepositoryPath, targetRepositoryPath.getRepository());
                }
                if (Files.exists(targetRepositoryPath)) {
                    return true;
                }
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            result = false;
            failureReason = ex.getMessage();
        } finally {
            securityUtils.clearAuthentication();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).sourceArtifactPath(sourceRepoPath).targetArtifactPath(targetRepoPath).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }
}
