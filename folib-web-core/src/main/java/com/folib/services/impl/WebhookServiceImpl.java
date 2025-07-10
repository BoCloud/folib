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

import cn.hutool.core.net.NetUtil;
import cn.hutool.core.util.IdUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.configuration.MutableWebhookConfiguration;
import com.folib.configuration.WebhookConfiguration;
import com.folib.entity.WebhookLog;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.forms.configuration.WebhookConfigurationForm;
import com.folib.mapper.WebhookLogMapper;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.WebhookService;
import com.folib.util.StripedLockUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
@Transactional
public class WebhookServiceImpl implements WebhookService {

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private WebhookLogMapper webhookLogMapper;

    @Autowired
    @Lazy
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Override
    public void addWebhookConfiguration(WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        MutableWebhookConfiguration mutableWebhookConfiguration = MutableWebhookConfiguration.builder().build();
        BeanUtils.copyProperties(webhookConfigurationForm, mutableWebhookConfiguration);
        if (StringUtils.isBlank(webhookConfigurationForm.getUuid())) {
            mutableWebhookConfiguration.setUuid(generatorUuid());
            webhookConfigurationForm.setUuid(mutableWebhookConfiguration.getUuid());
        }
        configurationManagementService.addWebhookConfiguration(mutableWebhookConfiguration);
    }

    @Override
    public void updateWebhookConfiguration(WebhookConfigurationForm webhookConfigurationForm) throws IOException {
        MutableWebhookConfiguration mutableWebhookConfiguration = MutableWebhookConfiguration.builder().build();
        BeanUtils.copyProperties(webhookConfigurationForm, mutableWebhookConfiguration);
        configurationManagementService.updateWebhookConfiguration(mutableWebhookConfiguration);
    }

    @Override
    public void deleteWebhookConfiguration(String uuid) throws IOException {
        WebhookConfiguration webhookConfiguration = configurationManagementService.getConfiguration().getWebhookConfiguration().get(uuid);
        if (Objects.nonNull(webhookConfiguration)) {
            configurationManagementService.deleteWebhookConfiguration(uuid);
            deleteWebhookLogByUrl(webhookConfiguration.getUrl());
        }
    }

    @Override
    public List<WebhookConfigurationForm> getWebhookConfiguration() throws IOException {
        return Optional.ofNullable(configurationManagementService.getConfiguration().getWebhookConfiguration()).orElse(Collections.emptyMap()).values().stream().map(WebhookConfigurationForm::fromConfiguration).collect(Collectors.toList());
    }

    @Override
    public void saveWebhookLog(WebhookLog webhookLog) {
        Lock lock = StripedLockUtils.lock(webhookLog.getUrl());
        try {
            if (lock.tryLock(30L, TimeUnit.SECONDS)) {
                try {
                    if (Objects.isNull(webhookLog.getCreateTime())) {
                        webhookLog.setCreateTime(new Date());
                    }
                    List<WebhookLog> webhookLogList = queryWebhookLogList(webhookLog);
                    int size = 50;
                    if (CollectionUtils.isNotEmpty(webhookLogList) && webhookLogList.size() >= size) {
                        //每个url只保留50条记录
                        String id = webhookLogList.get(size - 1).getId() + "";
                        webhookLogMapper.deleteById(id);
                    }
                    int artifactPathMaxLength = 255;
                    String artifactPath = webhookLog.getArtifactPath();
                    if (StringUtils.isNotBlank(artifactPath) && artifactPath.length() >= artifactPathMaxLength) {
                        webhookLog.setArtifactPath(artifactPath.substring(0, artifactPathMaxLength));
                    }
                    webhookLogMapper.insert(webhookLog);
                } finally {
                    lock.unlock();
                }
            } else {
                log.warn("保存webhook日志 {} 未获取到锁", JSONObject.toJSONString(webhookLog));
            }
        } catch (InterruptedException e) {
            log.error("保存webhook日志 {} 错误：{}", JSONObject.toJSONString(webhookLog), ExceptionUtils.getStackTrace(e));
            throw new RuntimeException(e);
        }
    }

    @Override
    public void testWebhook(WebhookConfigurationForm webhookConfigurationForm) {
        WebhookConfiguration webhookConfiguration = configurationManagementService.getConfiguration().getWebhookConfiguration().get(webhookConfigurationForm.getUuid());
        if (Objects.nonNull(webhookConfiguration)) {
            Set<String> events = webhookConfigurationForm.getEvents();
            webhookConfigurationForm = WebhookConfigurationForm.fromConfiguration(webhookConfiguration);
            webhookConfigurationForm.setEvents(events);
            handlerWebhookTest(webhookConfigurationForm);
        }
    }

    @Override
    public void deleteWebhookLog(WebhookLog webhookLog) {
        webhookLogMapper.deleteById(webhookLog.getId());
    }

    @Override
    public List<WebhookLog> queryWebhookLogList(WebhookLog webhookLog) {
        return webhookLogMapper.selectList(Wrappers.<WebhookLog>lambdaQuery()
                .eq(StringUtils.isNotBlank(webhookLog.getUrl()),WebhookLog::getUrl, webhookLog.getUrl())
                .orderByDesc(WebhookLog::getCreateTime)
        );
    }

    @Override
    public WebhookLog queryWebhookLog(WebhookLog webhookLog) {
        return webhookLogMapper.selectById(webhookLog);
    }

    private String generatorUuid() {
        return "webhook-" + IdUtil.fastSimpleUUID();
    }

    /**
     * 根据url删除webhook日志
     *
     * @param url url
     */
    private void deleteWebhookLogByUrl(String url) {
        webhookLogMapper.delete(Wrappers.<WebhookLog>lambdaQuery().eq(WebhookLog::getUrl, url));
    }

    public void handlerWebhookTest(WebhookConfigurationForm webhookConfiguration) {
        String instance = NetUtil.getLocalhost().getHostAddress();
        Map<String, String> headerMap = Maps.newHashMap();
        headerMap.put("Content-Type", "application/json");
        headerMap.put("User-Agent", "Folibrary");
        headerMap.put("X-Folibrary-Instance", instance);
        if (StringUtils.isNotBlank(webhookConfiguration.getAccessToken())) {
            headerMap.put("X-Folibrary-Token", webhookConfiguration.getAccessToken());
        }
        String artifactPath = "";
        JSONObject defaultBodyJson = new JSONObject();
        String body = "";
        List<String> events = Lists.newArrayList(ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.toString());
        for (String eventType : webhookConfiguration.getEvents()) {
            headerMap.put("X-Folibrary-Event", eventType);
            if (!events.contains(eventType)) {
                body = "{\"artifactFileExists\":true,\"artifactPath\":\"commons-io/commons-io/2.8.0/commons-io-2.8.0.jar\",\"checksums\":{\"SHA-1\":\"92999e26e6534606b5678014e66948286298a35c\",\"MD5\":\"21ba575792e2694c39af13918a80550b\"},\"created\":\"2023-03-16T11:11:40.559\",\"criticalVulnerabilitiesCount\":0,\"dependencyCount\":0,\"dependencyVulnerabilitiesCount\":0,\"downloadCount\":0,\"evidenceQuantity\":0,\"filePaths\":[\"/folib-server/folib-data/storages/folib-default/local-maven/commons-io/commons-io/2.8.0/commons-io-2.8.0.jar\"],\"highVulnerabilitiesCount\":0,\"lastUpdated\":\"2023-03-16T11:11:52.307\",\"lastUsed\":\"2023-03-16T11:11:40.559\",\"lowVulnerabilitiesCount\":0,\"mediumVulnerabilitiesCount\":0,\"nativeId\":6928,\"repositoryId\":\"local-maven\",\"safeLevel\":\"unScan\",\"sizeInBytes\":285424,\"storageId\":\"folib-default\",\"storageIdAndRepositoryId\":\"folib-default-local-maven\",\"suppressedVulnerabilitiesCount\":0,\"uuid\":\"folib-default-local-maven-commons-io/commons-io/2.8.0/commons-io-2.8.0.jar\",\"vulnerabilities\":[],\"vulnerabilitiesCount\":0,\"vulnerabilitySet\":[]}";
            } else {
                artifactPath = "commons-io/commons-io/2.8.0";
                defaultBodyJson.put("artifactPath", artifactPath);
                body = defaultBodyJson.toJSONString();
            }
            handlerWebhook(webhookConfiguration, "", "", "", eventType, body, headerMap);
        }
    }

    /**
     * 发送post请求
     *
     * @param url       url
     * @param body      body
     * @param headerMap headerMap
     * @return 响应
     */
    private Response doPost(String url, String body, Map<String, String> headerMap) {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request();
        for (Map.Entry<String, String> entry : headerMap.entrySet()) {
            builder = builder.header(entry.getKey(), entry.getValue());
        }
        return builder.post(Entity.entity(body, MediaType.APPLICATION_JSON));
    }

    @Override
    public void handlerWebhook(WebhookConfigurationForm webhookConfiguration, String storageId, String repositoryId, String artifactPath, String eventType, String body, Map<String, String> headerMap) {
        BigDecimal startTime = BigDecimal.valueOf(System.currentTimeMillis()), endTime, completionTime;
        if (StringUtils.isBlank(storageId)) {
            storageId = "folib-common";
        }
        if (StringUtils.isBlank(repositoryId)) {
            repositoryId = "local-maven";
        }
        if (StringUtils.isBlank(artifactPath)) {
            artifactPath = "commons-io/commons-io/2.8.0/commons-io-2.8.0.jar";
        }
        Response response = null;
        try {
            response = doPost(webhookConfiguration.getUrl(), body, headerMap);
            endTime = BigDecimal.valueOf(System.currentTimeMillis());
            endTime = endTime.subtract(startTime);
            completionTime = endTime.divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP);
            WebhookLog webhookLog = WebhookLog.builder().createTime(new Date()).eventType(eventType).storageId(storageId).repositoryId(repositoryId)
                    .artifactPath(artifactPath).url(webhookConfiguration.getUrl()).accessToken(webhookConfiguration.getAccessToken()).method("POST")
                    .requestHeaders(JSONObject.toJSONString(headerMap)).completionTime(completionTime.toString()).request(body).responseStatus(response.getStatus() + "").responseHeaders(JSONObject.toJSONString(response.getHeaders()))
                    .response(response.readEntity(String.class)).build();
            saveWebhookLog(webhookLog);
        } catch (Exception ex) {
            endTime = BigDecimal.valueOf(System.currentTimeMillis());
            endTime = endTime.subtract(startTime);
            completionTime = endTime.divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP);
            String message = ex.getMessage();
            if (message.length() > 255) {
                message = message.substring(0, 255);
            }
            log.error("事件监听，处理webhook，事件类型：{} repositoryPath：{} 错误：{}", eventType, artifactPath, ExceptionUtils.getStackTrace(ex));
            WebhookLog webhookLog = WebhookLog.builder().createTime(new Date()).eventType(eventType).storageId(storageId).repositoryId(repositoryId)
                    .artifactPath(artifactPath).url(webhookConfiguration.getUrl()).accessToken(webhookConfiguration.getAccessToken()).method("POST").url(webhookConfiguration.getUrl())
                    .requestHeaders(JSONObject.toJSONString(Objects.nonNull(headerMap) ? headerMap : "")).completionTime(completionTime.toString()).request(body).responseStatus("").responseHeaders("")
                    .response("").remark(message).build();
            saveWebhookLog(webhookLog);
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }
}
