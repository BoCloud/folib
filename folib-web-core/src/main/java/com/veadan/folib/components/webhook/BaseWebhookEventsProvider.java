package com.veadan.folib.components.webhook;

import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.components.syncartifact.SyncUtils;
import com.veadan.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.veadan.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.veadan.folib.domain.ArtifactMetadata;
import com.veadan.folib.domain.migrate.ArtifactMigrateInfo;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.WebhookEventsLog;
import com.veadan.folib.enums.JFrogEventTypeEnum;
import com.veadan.folib.enums.WebhookEventsStatusEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.WebhookEventsLogService;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.utils.SecurityUtils;
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

    protected ArtifactResolutionService artifactResolutionService;

    protected ArtifactManagementService artifactManagementService;

    protected SecurityUtils securityUtils;

    protected WebhookEventsLogService webhookEventsLogService;

    private final Set<String> EXCLUDE_PROPERTIES = Set.of("sha256");

    public BaseWebhookEventsProvider(ArtifactResolutionService artifactResolutionService, ArtifactManagementService artifactManagementService, SecurityUtils securityUtils, WebhookEventsLogService webhookEventsLogService) {
        this.artifactResolutionService = artifactResolutionService;
        this.artifactManagementService = artifactManagementService;
        this.securityUtils = securityUtils;
        this.webhookEventsLogService = webhookEventsLogService;
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

    private void handlerProperties(ArtifactMigrateInfo artifactMigrateInfo, String repoKey, String storageId, String repositoryId, String artifactPath) {
        String properties = getPropertiesByKeyAndPath(artifactMigrateInfo, repoKey, artifactPath);
        if (StringUtils.isNotBlank(properties)) {
            SyncUtils syncUtils = SpringUtil.getBean(SyncUtils.class);
            syncUtils.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
        }
    }

}
