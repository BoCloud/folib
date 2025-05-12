package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.entity.WebhookEventsLog;
import com.veadan.folib.enums.WebhookEventsStatusEnum;
import com.veadan.folib.mapper.WebhookEventsLogMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.WebhookEventsLogService;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2025/3/6
 **/
@Slf4j
@Service
@Transactional(rollbackFor = Exception.class)
public class WebhookEventsLogServiceImpl implements WebhookEventsLogService {

    @Autowired
    private IdGenerateUtils idGenerateUtils;

    @Autowired
    private WebhookEventsLogMapper webhookEventsLogMapper;

    @Autowired
    private DistributedLockComponent distributedLockComponent;

    @Override
    public void saveWebhookEventsLog(WebhookEventsLog webhookEventsLog, int type) {
        String lockName = "SAVE_WEBHOOK_EVENTS_LOG_LOCK_" + webhookEventsLog.getSha256Checksum();
        long waitTime = 6L;
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                WebhookEventsLog webhookEventsLogExists = WebhookEventsLog.builder().eventType(webhookEventsLog.getEventType()).eventRepositoryId(webhookEventsLog.getEventRepositoryId())
                        .storageId(webhookEventsLog.getStorageId()).repositoryId(webhookEventsLog.getRepositoryId()).sha256Checksum(webhookEventsLog.getSha256Checksum()).artifactPath(webhookEventsLog.getArtifactPath()).build();
                webhookEventsLogExists = queryWebhookEventsLog(webhookEventsLogExists);
                Date date = new Date();
                if (Objects.isNull(webhookEventsLogExists)) {
                    webhookEventsLog.setId(idGenerateUtils.generateId("webhookEventsLogId"));
                    webhookEventsLog.setCreateBy(UserUtils.getUsername());
                    webhookEventsLog.setCreateTime(date);
                    webhookEventsLogMapper.insert(webhookEventsLog);
                } else if (type != 1) {
                    WebhookEventsLog updateWebhookEventsLog = WebhookEventsLog.builder().id(webhookEventsLogExists.getId()).retryCount(webhookEventsLogExists.getRetryCount() + 1)
                            .retryTime(date).updateBy(UserUtils.getUsername()).updateTime(date).failureReason(webhookEventsLog.getFailureReason()).build();
                    updateWebhookEventsLog(updateWebhookEventsLog);
                }
            } finally {
                distributedLockComponent.unLock(lockName);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    @Override
    public void updateWebhookEventsLog(WebhookEventsLog webhookEventsLog) {
        WebhookEventsLog existsWebhookEventsLog = webhookEventsLogMapper.selectById(webhookEventsLog.getId());
        if (Objects.nonNull(existsWebhookEventsLog)) {
            webhookEventsLogMapper.updateById(webhookEventsLog);
        }
    }

    @Override
    public void deleteWebhookEventsLog(WebhookEventsLog webhookEventsLog) {
        WebhookEventsLog existsWebhookEventsLog = queryWebhookEventsLog(webhookEventsLog);
        if (Objects.isNull(existsWebhookEventsLog)) {
            throw new RuntimeException(String.format("WebhookEvents [%s] not found", webhookEventsLog.getId()));
        }
        webhookEventsLogMapper.deleteById(webhookEventsLog.getId());
    }

    @Override
    public List<WebhookEventsLog> queryWebhookEventsLogList(List<Integer> statsList, WebhookEventsLog webhookEventsLog) {
        return webhookEventsLogMapper.selectList(Wrappers.<WebhookEventsLog>lambdaQuery()
                .eq(StringUtils.isNotBlank(webhookEventsLog.getEventType()), WebhookEventsLog::getEventType, webhookEventsLog.getEventType())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getEventRepositoryId()), WebhookEventsLog::getEventRepositoryId, webhookEventsLog.getEventRepositoryId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getStorageId()), WebhookEventsLog::getStorageId, webhookEventsLog.getStorageId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getRepositoryId()), WebhookEventsLog::getRepositoryId, webhookEventsLog.getRepositoryId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getArtifactName()), WebhookEventsLog::getArtifactName, webhookEventsLog.getArtifactName())
                .eq(Objects.nonNull(webhookEventsLog.getStatus()), WebhookEventsLog::getStatus, webhookEventsLog.getStatus())
                .eq(Objects.nonNull(webhookEventsLog.getRetry()), WebhookEventsLog::getRetry, webhookEventsLog.getRetry())
                .le(Objects.nonNull(webhookEventsLog.getRetryCount()), WebhookEventsLog::getRetryCount, webhookEventsLog.getRetryCount())
                .in(CollectionUtils.isNotEmpty(statsList),WebhookEventsLog::getStatus, statsList)
                .orderByDesc(WebhookEventsLog::getCreateTime)
        );
    }

    @Override
    public WebhookEventsLog queryWebhookEventsLog(WebhookEventsLog webhookEventsLog) {
        List<WebhookEventsLog> webhookEventsLogs = webhookEventsLogMapper.selectList(Wrappers.<WebhookEventsLog>lambdaQuery()
                .eq(StringUtils.isNotBlank(webhookEventsLog.getEventType()), WebhookEventsLog::getEventType, webhookEventsLog.getEventType())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getEventRepositoryId()), WebhookEventsLog::getEventRepositoryId, webhookEventsLog.getEventRepositoryId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getStorageId()), WebhookEventsLog::getStorageId, webhookEventsLog.getStorageId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getRepositoryId()), WebhookEventsLog::getRepositoryId, webhookEventsLog.getRepositoryId())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getArtifactName()), WebhookEventsLog::getArtifactName, webhookEventsLog.getArtifactName())
               .eq(StringUtils.isNotBlank(webhookEventsLog.getArtifactPath()), WebhookEventsLog::getArtifactPath, webhookEventsLog.getArtifactPath())
                .eq(StringUtils.isNotBlank(webhookEventsLog.getSha256Checksum()), WebhookEventsLog::getSha256Checksum, webhookEventsLog.getSha256Checksum())
                .eq(Objects.nonNull(webhookEventsLog.getStatus()), WebhookEventsLog::getStatus, webhookEventsLog.getStatus())
                .eq(Objects.nonNull(webhookEventsLog.getRetry()), WebhookEventsLog::getRetry, webhookEventsLog.getRetry())
                .eq(Objects.nonNull(webhookEventsLog.getId()), WebhookEventsLog::getId, webhookEventsLog.getId())
        );
        return CollectionUtils.isNotEmpty(webhookEventsLogs) ? webhookEventsLogs.get(0) : null;
    }

    @Override
    public long count(List<Integer> statsList, Integer retryCount) {
        return webhookEventsLogMapper.selectCount(Wrappers.<WebhookEventsLog>lambdaQuery()
                .in(WebhookEventsLog::getStatus, statsList)
                .le(WebhookEventsLog::getRetryCount, retryCount)
        );
    }

    @Override
    public void deleteSuccessLog() {
        webhookEventsLogMapper.delete(Wrappers.<WebhookEventsLog>lambdaQuery().eq(WebhookEventsLog::getStatus, WebhookEventsStatusEnum.SUCCESS.getStatus()));
    }

    @Override
    public TableResultResponse<WebhookEventsLog> queryWebhookEventLogPage(Integer page, Integer limit, List<Integer> statsList, WebhookEventsLog webhookEventsLog) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<WebhookEventsLog> webhookEventsLogs = queryWebhookEventsLogList(statsList, webhookEventsLog);
        return new TableResultResponse<WebhookEventsLog>(result.getTotal(), CollectionUtils.isEmpty(webhookEventsLogs) ? Collections.emptyList() : webhookEventsLogs);
    }

}
