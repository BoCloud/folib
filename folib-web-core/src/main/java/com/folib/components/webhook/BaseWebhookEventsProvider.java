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

import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.folib.components.syncartifact.SyncUtils;
import com.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.folib.domain.ArtifactMetadata;
import com.folib.domain.migrate.ArtifactMigrateInfo;
import com.folib.entity.Dict;
import com.folib.entity.WebhookEventsLog;
import com.folib.enums.JFrogEventTypeEnum;
import com.folib.enums.WebhookEventsStatusEnum;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.WebhookEventsLogService;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.utils.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.RepositoryHandle;

import java.io.InputStream;
import java.nio.file.Files;
import java.util.*;

/**
 * @author leipenghui
 * @date 2025/3/7
 */
@Slf4j
public abstract class BaseWebhookEventsProvider implements WebhookEventsProvider {

    protected RepositoryPathResolver repositoryPathResolver;

    protected ArtifactResolutionService artifactResolutionService;

    protected ArtifactManagementService artifactManagementService;

    protected SecurityUtils securityUtils;

    protected WebhookEventsLogService webhookEventsLogService;

    protected PromotionUtil promotionUtil;

    private final Set<String> EXCLUDE_PROPERTIES = Set.of("sha256");

    public BaseWebhookEventsProvider(RepositoryPathResolver repositoryPathResolver, ArtifactResolutionService artifactResolutionService, ArtifactManagementService artifactManagementService, SecurityUtils securityUtils, WebhookEventsLogService webhookEventsLogService, PromotionUtil promotionUtil) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactResolutionService = artifactResolutionService;
        this.artifactManagementService = artifactManagementService;
        this.securityUtils = securityUtils;
        this.webhookEventsLogService = webhookEventsLogService;
        this.promotionUtil = promotionUtil;
    }

    @Override
    public abstract void register();

    @Override
    public boolean handler(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = false;
        if (JFrogEventTypeEnum.DEPLOYED.getType().equalsIgnoreCase(webhook.getEventType())) {
            result = deployedEvent(webhook, repositoryPath, artifactMigrateInfo, type);
        } else if (JFrogEventTypeEnum.DELETED.getType().equalsIgnoreCase(webhook.getEventType())) {
            result = deletedEvent(webhook, repositoryPath, type);
        } else if (JFrogEventTypeEnum.MOVED.getType().equalsIgnoreCase(webhook.getEventType())) {
            result = movedEvent(webhook, repositoryPath, artifactMigrateInfo, type);
        } else if (JFrogEventTypeEnum.COPIED.getType().equalsIgnoreCase(webhook.getEventType())) {
            result = copiedEvent(webhook, repositoryPath, artifactMigrateInfo, type);
        }
        return result;
    }

    @Override
    public String resolvePath(WebhookDto webhook) {
        return webhook.getData().getPath();
    }

    protected boolean deployedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = false;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(), failureReason = "";
        try {
            ArtifactMigrateInfo jfrogInfo = JSON.parseObject(artifactMigrateInfo.getAlias(), ArtifactMigrateInfo.class);
            if (RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType())) {
                if (Files.exists(repositoryPath)) {
                    return true;
                }
                // 获取制品
                try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(jfrogInfo.getRemotePreUrl()).setUsername(jfrogInfo.getUsername()).setPassword(jfrogInfo.getPassword()).build()) {
                    // 访问远程仓库
                    securityUtils.setAdminAuthentication();
                    RepositoryHandle repository = artifactory.repository(webhook.getData().getRepoKey());
                    try (InputStream artifactStream = repository.download(artifactData.getPath()).doDownload()) {
                        artifactManagementService.store(repositoryPath, artifactStream);
                    }
                } catch (Exception ex) {
                    log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
                    failureReason = ex.getMessage();
                } finally {
                    securityUtils.clearAuthentication();
                }
            } else {
                artifactResolutionService.resolvePath(repositoryPath);
            }
            if (Files.exists(repositoryPath)) {
                handlerProperties(jfrogInfo, artifactData.getRepoKey(), storageId, repositoryId, path);
                result = true;
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            failureReason = ex.getMessage();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    protected boolean deletedEvent(WebhookDto webhook, RepositoryPath repositoryPath, int type) {
        if (checkNotExists(repositoryPath)) {
            return true;
        }
        boolean result = false;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(), failureReason = "";
        try {
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
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            result = false;
            failureReason = ex.getMessage();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    protected boolean checkNotExists(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath)) {
            return true;
        }
        if (!Files.exists(repositoryPath)) {
            return true;
        }
        return false;
    }

    public String getPropertiesByKeyAndPath(ArtifactMigrateInfo artifactMigrateInfo, String repoKey, String path) {
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(artifactMigrateInfo.getRemotePreUrl()).setUsername(artifactMigrateInfo.getUsername()).setPassword(artifactMigrateInfo.getPassword()).build()) {
            Map<String, List<String>> properties = artifactory.repository(repoKey).file(path).getProperties();
            HashMap<String, ArtifactMetadata> result = new HashMap<>();
            for (String key : properties.keySet()) {
                if (EXCLUDE_PROPERTIES.contains(key)) {
                    continue;
                }
                List<String> strings = properties.get(key);
                if (strings.size() == 1) {
                    ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().value(strings.get(0)).type("STRING").viewShow(1).build();
                    result.put(key, artifactMetadata);
                }
            }
            if (result.isEmpty()) {
                return null;
            }
            return JSON.toJSONString(result);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }

    protected boolean movedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = false;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(),
                sourceRepoPath = artifactData.getSourceRepoPath(), targetRepoPath = artifactData.getTargetRepoPath(), targetRepositoryId = "", targetArtifactPath = "", failureReason = "";
        try {
            targetRepositoryId = targetRepoPath.split("/")[0];
            targetArtifactPath = targetRepoPath.replace(targetRepositoryId + "/", "");
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(storageId, targetRepositoryId, targetArtifactPath);
            ArtifactMigrateInfo jfrogInfo = JSON.parseObject(artifactMigrateInfo.getAlias(), ArtifactMigrateInfo.class);
            if (Files.exists(repositoryPath)) {
                // 源存在，执行move
                promotionUtil.executeFastSyncMove(repositoryPath, repositoryPath.getRepository(), targetRepositoryPath, targetRepositoryPath.getRepository());
            } else {
                // 源不存在，去JFrog拉取
                try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(jfrogInfo.getRemotePreUrl()).setUsername(jfrogInfo.getUsername()).setPassword(jfrogInfo.getPassword()).build()) {
                    // 访问远程仓库
                    securityUtils.setAdminAuthentication();
                    RepositoryHandle repository = artifactory.repository(targetRepositoryId);
                    try (InputStream artifactStream = repository.download(targetArtifactPath).doDownload()) {
                        artifactManagementService.store(targetRepositoryPath, artifactStream);
                    }
                } catch (Exception ex) {
                    log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
                    failureReason = ex.getMessage();
                } finally {
                    securityUtils.clearAuthentication();
                }
            }
            if (Files.exists(targetRepositoryPath)) {
                RepositoryFiles.delete(repositoryPath, true);
                handlerProperties(jfrogInfo, targetRepositoryId, storageId, targetRepositoryId, targetArtifactPath);
                result = true;
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            failureReason = ex.getMessage();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).sourceArtifactPath(sourceRepoPath).targetArtifactPath(targetRepoPath).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    protected boolean copiedEvent(WebhookDto webhook, RepositoryPath repositoryPath, Dict artifactMigrateInfo, int type) {
        boolean result = false;
        ArtifactData artifactData = webhook.getData();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), name = repositoryPath.getFileName().toString(), path = artifactData.getPath(),
                sourceRepoPath = artifactData.getSourceRepoPath(), targetRepoPath = artifactData.getTargetRepoPath(), targetRepositoryId = "", targetArtifactPath = "", failureReason = "";
        try {
            targetRepositoryId = targetRepoPath.split("/")[0];
            targetArtifactPath = targetRepoPath.replace(targetRepositoryId + "/", "");
            RepositoryPath targetRepositoryPath = repositoryPathResolver.resolve(storageId, targetRepositoryId, targetArtifactPath);
            ArtifactMigrateInfo jfrogInfo = JSON.parseObject(artifactMigrateInfo.getAlias(), ArtifactMigrateInfo.class);
            if (Files.exists(repositoryPath)) {
                // 源存在，执行copy
                promotionUtil.executeFastSyncCopy(repositoryPath, repositoryPath.getRepository(), targetRepositoryPath, targetRepositoryPath.getRepository());
            } else {
                // 源不存在，去JFrog拉取
                try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(jfrogInfo.getRemotePreUrl()).setUsername(jfrogInfo.getUsername()).setPassword(jfrogInfo.getPassword()).build()) {
                    // 访问远程仓库
                    securityUtils.setAdminAuthentication();
                    RepositoryHandle repository = artifactory.repository(targetRepositoryId);
                    try (InputStream artifactStream = repository.download(targetArtifactPath).doDownload()) {
                        artifactManagementService.store(targetRepositoryPath, artifactStream);
                    }
                } catch (Exception ex) {
                    log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
                    failureReason = ex.getMessage();
                } finally {
                    securityUtils.clearAuthentication();
                }
            }
            if (Files.exists(targetRepositoryPath)) {
                handlerProperties(jfrogInfo, targetRepositoryId, storageId, targetRepositoryId, targetArtifactPath);
                result = true;
            }
        } catch (Exception ex) {
            log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", artifactData.getRepoKey(), storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
            failureReason = ex.getMessage();
        }
        if (!result) {
            WebhookEventsLog webhookEventsLog = WebhookEventsLog.builder().eventType(webhook.getEventType()).eventRepositoryId(artifactData.getRepoKey()).storageId(storageId).repositoryId(repositoryId).artifactName(name)
                    .artifactPath(path).sha256Checksum(artifactData.getSha256()).size(artifactData.getSize()).sourceArtifactPath(sourceRepoPath).targetArtifactPath(targetRepoPath).status(WebhookEventsStatusEnum.FAILURE.getStatus()).failureReason(failureReason).build();
            webhookEventsLogService.saveWebhookEventsLog(webhookEventsLog, type);
        }
        return result;
    }

    private void handlerProperties(ArtifactMigrateInfo artifactMigrateInfo, String repoKey, String storageId, String repositoryId, String artifactPath) {
        String properties = getPropertiesByKeyAndPath(artifactMigrateInfo, repoKey, artifactPath);
        if (StringUtils.isNotBlank(properties)) {
            SyncUtils syncUtils = SpringUtil.getBean(SyncUtils.class);
            syncUtils.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
        }
    }

}
