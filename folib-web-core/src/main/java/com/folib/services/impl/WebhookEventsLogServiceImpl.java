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
package com.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.folib.components.DistributedLockComponent;
import com.folib.components.IdGenerateUtils;
import com.folib.entity.WebhookEventsLog;
import com.folib.enums.WebhookEventsStatusEnum;
import com.folib.mapper.WebhookEventsLogMapper;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.WebhookEventsLogService;
import com.folib.utils.UserUtils;
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
