package com.veadan.folib.services.impl;

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
import tk.mybatis.mapper.entity.Example;

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
                    webhookEventsLogMapper.insertSelective(webhookEventsLog);
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
        WebhookEventsLog existsWebhookEventsLog = webhookEventsLogMapper.selectByPrimaryKey(webhookEventsLog.getId());
        if (Objects.nonNull(existsWebhookEventsLog)) {
            webhookEventsLogMapper.updateByPrimaryKeySelective(webhookEventsLog);
        }
    }

    @Override
    public void deleteWebhookEventsLog(WebhookEventsLog webhookEventsLog) {
        WebhookEventsLog existsWebhookEventsLog = queryWebhookEventsLog(webhookEventsLog);
        if (Objects.isNull(existsWebhookEventsLog)) {
            throw new RuntimeException(String.format("WebhookEvents [%s] not found", webhookEventsLog.getId()));
        }
        webhookEventsLogMapper.deleteByPrimaryKey(webhookEventsLog.getId());
    }

    @Override
    public List<WebhookEventsLog> queryWebhookEventsLogList(List<Integer> statsList, WebhookEventsLog webhookEventsLog) {
        Example example = Example.builder(WebhookEventsLog.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (StringUtils.isNotBlank(webhookEventsLog.getEventType())) {
            criteria.andEqualTo("eventType", webhookEventsLog.getEventType());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getEventRepositoryId())) {
            criteria.andEqualTo("eventRepositoryId", webhookEventsLog.getEventRepositoryId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getStorageId())) {
            criteria.andEqualTo("storageId", webhookEventsLog.getStorageId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getRepositoryId())) {
            criteria.andEqualTo("repositoryId", webhookEventsLog.getRepositoryId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getArtifactName())) {
            criteria.andEqualTo("artifactName", webhookEventsLog.getArtifactName());
        }
        if (Objects.nonNull(webhookEventsLog.getStatus())) {
            criteria.andEqualTo("status", webhookEventsLog.getStatus());
        }
        if (Objects.nonNull(webhookEventsLog.getRetry())) {
            criteria.andEqualTo("retry", webhookEventsLog.getRetry());
        }
        if (Objects.nonNull(webhookEventsLog.getRetryCount())) {
            criteria.andLessThanOrEqualTo("retryCount", webhookEventsLog.getRetryCount());
        }
        if (CollectionUtils.isNotEmpty(statsList)) {
            criteria.andIn("status", statsList);
        }
        example.setOrderByClause("create_time desc");
        return webhookEventsLogMapper.selectByExample(example);
    }

    @Override
    public WebhookEventsLog queryWebhookEventsLog(WebhookEventsLog webhookEventsLog) {
        Example example = Example.builder(WebhookEventsLog.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (StringUtils.isNotBlank(webhookEventsLog.getEventType())) {
            criteria.andEqualTo("eventType", webhookEventsLog.getEventType());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getEventRepositoryId())) {
            criteria.andEqualTo("eventRepositoryId", webhookEventsLog.getEventRepositoryId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getStorageId())) {
            criteria.andEqualTo("storageId", webhookEventsLog.getStorageId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getRepositoryId())) {
            criteria.andEqualTo("repositoryId", webhookEventsLog.getRepositoryId());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getArtifactName())) {
            criteria.andEqualTo("artifactName", webhookEventsLog.getArtifactName());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getArtifactPath())) {
            criteria.andEqualTo("artifactPath", webhookEventsLog.getArtifactPath());
        }
        if (StringUtils.isNotBlank(webhookEventsLog.getSha256Checksum())) {
            criteria.andEqualTo("sha256Checksum", webhookEventsLog.getSha256Checksum());
        }
        if (Objects.nonNull(webhookEventsLog.getStatus())) {
            criteria.andEqualTo("status", webhookEventsLog.getStatus());
        }
        if (Objects.nonNull(webhookEventsLog.getRetry())) {
            criteria.andEqualTo("retry", webhookEventsLog.getRetry());
        }
        if (Objects.nonNull(webhookEventsLog.getId())) {
            criteria.andEqualTo("id", webhookEventsLog.getId());
        }
        List<WebhookEventsLog> webhookEventsLogs = webhookEventsLogMapper.selectByExample(example);
        return CollectionUtils.isNotEmpty(webhookEventsLogs) ? webhookEventsLogs.get(0) : null;
    }

    @Override
    public long count(List<Integer> statsList, Integer retryCount) {
        Example example = Example.builder(WebhookEventsLog.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andIn("status", statsList);
        criteria.andLessThanOrEqualTo("retryCount", retryCount);
        return webhookEventsLogMapper.selectCountByExample(example);
    }

    @Override
    public void deleteSuccessLog() {
        webhookEventsLogMapper.delete(WebhookEventsLog.builder().status(WebhookEventsStatusEnum.SUCCESS.getStatus()).build());
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
