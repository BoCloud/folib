package com.veadan.folib.task;


import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.webhook.WebhookEventsProvider;
import com.veadan.folib.components.webhook.WebhookEventsProviderRegistry;
import com.veadan.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.veadan.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.WebhookEventsLog;
import com.veadan.folib.enums.WebhookEventsStatusEnum;
import com.veadan.folib.enums.WebhookEventsTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.WebhookEventsLogService;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * webhook事件task
 */
@Slf4j
@Component
@EnableScheduling
public class WebhookEventsLogTask {

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private WebhookEventsLogService webhookEventsLogService;

    @Inject
    @Lazy
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    @Lazy
    protected WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private DictService dictService;

    /**
     * 每3分钟
     */
    @Scheduled(cron = "0 0/3 * * * ? ")
    public void run() {
        String lockName = "WebhookEventsLogTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        long startTime = System.currentTimeMillis();
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                webhookEventsLogService.deleteSuccessLog();
                List<Integer> statsList = Lists.newArrayList(WebhookEventsStatusEnum.INIT.getStatus(), WebhookEventsStatusEnum.FAILURE.getStatus());
                long totalCount = webhookEventsLogService.count(statsList, getMaxRetryCount());
                log.info("Webhook events log find total [{}]", totalCount);
                if (totalCount <= 0) {
                    return;
                }
                int batchSize = 10;
                // 计算总页数
                int totalPages = (int) Math.ceil((double) totalCount / batchSize);
                for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
                    log.info("Webhook events totalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
                    TableResultResponse<WebhookEventsLog> webhookEventsLogTableResultResponse = webhookEventsLogService.queryWebhookEventLogPage(currentPage, batchSize, statsList, WebhookEventsLog.builder().retryCount(getMaxRetryCount()).build());
                    List<WebhookEventsLog> webhookEventsLogs = webhookEventsLogTableResultResponse.getData().getRows();
                    String storageId = "", repositoryId = "";
                    RepositoryPath repositoryPath;
                    WebhookDto webhook;
                    ArtifactData artifactData = null;
                    if (CollectionUtils.isNotEmpty(webhookEventsLogs)) {
                        for (WebhookEventsLog webhookEventsLog : webhookEventsLogs) {
                            try {
                                storageId = webhookEventsLog.getStorageId();
                                repositoryId = webhookEventsLog.getRepositoryId();
                                artifactData = ArtifactData.builder().name(webhookEventsLog.getArtifactName()).path(webhookEventsLog.getArtifactPath())
                                        .repoKey(webhookEventsLog.getEventRepositoryId()).sha256(webhookEventsLog.getSha256Checksum()).size(webhookEventsLog.getSize()).sourceRepoPath(webhookEventsLog.getSourceArtifactPath())
                                        .targetRepoPath(webhookEventsLog.getTargetArtifactPath()).build();
                                repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, webhookEventsLog.getArtifactPath());
                                WebhookEventsProvider webhookEventsProvider = webhookEventsProviderRegistry.getProvider(WebhookEventsTypeEnum.resolveType(repositoryPath.getRepository().getLayout()));
                                webhook = WebhookDto.builder().eventType(webhookEventsLog.getEventType()).data(artifactData).build();
                                boolean result = webhookEventsProvider.handler(webhook, repositoryPath, getArtifactMigrateTask(), 2);
                                log.info("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] handler result [{}]", artifactData.getRepoKey(), storageId, repositoryId, artifactData.getPath(), result);
                                if (result) {
                                    webhookEventsLogService.updateWebhookEventsLog(WebhookEventsLog.builder().id(webhookEventsLog.getId()).updateBy("WebhookEventsLogTask").updateTime(new Date()).status(WebhookEventsStatusEnum.SUCCESS.getStatus()).build());
                                }
                            } catch (Exception ex) {
                                log.error("Webhook event handler eventRepositoryId [{}] storageId [{}] repositoryId [{}] path [{}] error [{}]", webhookEventsLog.getEventRepositoryId(), storageId, repositoryId, webhookEventsLog.getArtifactPath(), ExceptionUtils.getStackTrace(ex));
                            }
                        }
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("Scheduled webhookEventsLogTask end take time [{}] ms", System.currentTimeMillis() - startTime);
    }

    private Integer getMaxRetryCount() {
        int maxRetryCount = 2;
        String key = "WEBHOOK_EVENTS_LOG_TASK_MAX_RETRY_COUNT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            maxRetryCount = Integer.parseInt(value);
        }
        return maxRetryCount;
    }

    private Dict getArtifactMigrateTask() {
        Dict dict = new Dict();
        dict.setDictType("artifact_migrate_task");
        dict = dictService.selectLatestOneDict(dict);
        if (Objects.isNull(dict)) {
            log.error("Cannot find JFrog artifact migrate info");
        }
        return dict;
    }

}
