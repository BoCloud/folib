package com.veadan.folib.services;

import com.veadan.folib.entity.WebhookLog;
import com.veadan.folib.dto.configuration.WebhookConfigurationDto;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 **/
public interface WebhookService {

    /**
     * 新增webhook配置信息
     *
     * @param webhookConfigurationForm 参数
     * @throws IOException io异常
     */
    void addWebhookConfiguration(WebhookConfigurationDto webhookConfigurationForm) throws IOException;

    /**
     * 更新webhook配置信息
     *
     * @param webhookConfigurationForm 参数
     * @throws IOException io异常
     */
    void updateWebhookConfiguration(WebhookConfigurationDto webhookConfigurationForm) throws IOException;

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
    List<WebhookConfigurationDto> getWebhookConfiguration() throws IOException;

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
    void testWebhook(WebhookConfigurationDto webhookConfigurationForm);

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
    void handlerWebhook(WebhookConfigurationDto webhookConfigurationForm, String storageId, String repositoryId, String artifactPath, String eventType, String body, Map<String, String> headerMap);
}
