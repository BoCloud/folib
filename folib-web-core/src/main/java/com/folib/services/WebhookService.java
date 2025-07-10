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
package com.folib.services;

import com.folib.entity.WebhookLog;
import com.folib.forms.configuration.WebhookConfigurationForm;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * @author leipenghui
 **/
public interface WebhookService {

    /**
     * 新增webhook配置信息
     *
     * @param webhookConfigurationForm 参数
     * @throws IOException io异常
     */
    void addWebhookConfiguration(WebhookConfigurationForm webhookConfigurationForm) throws IOException;

    /**
     * 更新webhook配置信息
     *
     * @param webhookConfigurationForm 参数
     * @throws IOException io异常
     */
    void updateWebhookConfiguration(WebhookConfigurationForm webhookConfigurationForm) throws IOException;

    /**
     * 删除webhook配置信息
     *
     * @param uuid 参数
     * @throws IOException io异常
     */
    void deleteWebhookConfiguration(String uuid) throws IOException;

    /**
     * 查询webhook
     *
     * @return webhook 列表
     * @throws IOException 异常
     */
    List<WebhookConfigurationForm> getWebhookConfiguration() throws IOException;

    /**
     * 新增WebhookLog
     *
     * @param webhookLog 日志
     */
    void saveWebhookLog(WebhookLog webhookLog);

    /**
     * 测试webhook
     *
     * @param webhookConfigurationForm 参数
     */
    void testWebhook(WebhookConfigurationForm webhookConfigurationForm);

    /**
     * 删除WebhookLog
     *
     * @param webhookLog 日志
     */
    void deleteWebhookLog(WebhookLog webhookLog);

    /**
     * 查询WebhookLog列表
     *
     * @param webhookLog 日志
     * @return webhookLog列表
     */
    List<WebhookLog> queryWebhookLogList(WebhookLog webhookLog);

    /**
     * 查询WebhookLog
     *
     * @param webhookLog 日志
     * @return webhookLog
     */
    WebhookLog queryWebhookLog(WebhookLog webhookLog);

    /**
     * 处理webhook
     *
     * @param webhookConfigurationForm webhook配置
     * @param storageId            存储空间
     * @param repositoryId         仓库名称
     * @param artifactPath         制品路径
     * @param eventType            事件类型
     * @param body                 请求体
     * @param headerMap            headers
     */
    void handlerWebhook(WebhookConfigurationForm webhookConfigurationForm, String storageId, String repositoryId, String artifactPath, String eventType, String body, Map<String, String> headerMap);
}
