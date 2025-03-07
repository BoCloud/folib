package com.veadan.folib.services;

import com.veadan.folib.entity.WebhookEventsLog;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface WebhookEventsLogService {

    /**
     * 新增WebhookEventsLog
     *
     * @param webhookEventsLog 日志
     */
    void saveWebhookEventsLog(WebhookEventsLog webhookEventsLog);

    /**
     * 更新WebhookEventsLog
     *
     * @param webhookEventsLog 日志
     */
    void updateWebhookEventsLog(WebhookEventsLog webhookEventsLog);

    /**
     * 删除WebhookEventsLog
     *
     * @param webhookEventsLog 日志
     */
    void deleteWebhookEventsLog(WebhookEventsLog webhookEventsLog);

    /**
     * 查询WebhookEventsLog列表
     *
     * @param webhookEventsLog 日志
     * @return webhookEventsLog列表
     */
    List<WebhookEventsLog> queryWebhookEventsLogList(WebhookEventsLog webhookEventsLog);

    /**
     * 查询WebhookEventsLog
     *
     * @param webhookEventsLog 日志
     * @return webhookEventsLog
     */
    WebhookEventsLog queryWebhookEventsLog(WebhookEventsLog webhookEventsLog);

    /**
     * 查询总数
     *
     * @param status     状态
     * @param retryCount 重试次数
     * @return 总数
     */
    long count(Integer status, Integer retryCount);

    /**
     * 删除处理成功的日志
     */
    void deleteSuccessLog();

    /**
     * 分页查询
     *
     * @param page             页码
     * @param limit            每页数量
     * @param webhookEventsLog 查询参数
     * @return 分页结果
     */
    TableResultResponse<WebhookEventsLog> queryWebhookEventLogPage(Integer page, Integer limit, WebhookEventsLog webhookEventsLog);
}
