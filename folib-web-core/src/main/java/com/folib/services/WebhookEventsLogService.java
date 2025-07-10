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

import com.folib.entity.WebhookEventsLog;
import com.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface WebhookEventsLogService {

    /**
     * 新增WebhookEventsLog
     *
     * @param webhookEventsLog 日志
     * @param type             类型
     */
    void saveWebhookEventsLog(WebhookEventsLog webhookEventsLog, int type);

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
     * @param statsList        状态列表
     * @param webhookEventsLog 日志
     * @return webhookEventsLog列表
     */
    List<WebhookEventsLog> queryWebhookEventsLogList(List<Integer> statsList, WebhookEventsLog webhookEventsLog);

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
     * @param statsList  状态列表
     * @param retryCount 重试次数
     * @return 总数
     */
    long count(List<Integer> statsList, Integer retryCount);

    /**
     * 删除处理成功的日志
     */
    void deleteSuccessLog();

    /**
     * 分页查询
     *
     * @param page             页码
     * @param limit            每页数量
     * @param statsList        状态列表
     * @param webhookEventsLog 查询参数
     * @return 分页结果
     */
    TableResultResponse<WebhookEventsLog> queryWebhookEventLogPage(Integer page, Integer limit, List<Integer> statsList, WebhookEventsLog webhookEventsLog);
}
