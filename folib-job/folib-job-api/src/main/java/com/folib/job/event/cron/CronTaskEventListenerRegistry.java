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
package com.folib.job.event.cron;

import com.folib.event.AbstractEventListenerRegistry;

import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author Veadan
 */
@Component
public class CronTaskEventListenerRegistry
        extends AbstractEventListenerRegistry
{

    private static final Logger logger = LoggerFactory.getLogger(CronTaskEventListenerRegistry.class);


    public void dispatchCronTaskCreatedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_SAVED.getType(), uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_SAVED event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskDeletedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_DELETED.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_DELETED event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskExecutingEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTING.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTING event for '{}'...", uuid);

        dispatchEvent(event);
    }

    public void dispatchCronTaskExecutedEvent(final UUID uuid)
    {
        CronTaskEvent event = new CronTaskEvent(CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTION_COMPLETE.getType(),
                                                uuid.toString());

        logger.debug("Dispatching CronTaskEventTypeEnum.EVENT_CRON_TASK_EXECUTION_COMPLETE event for '{}'...", uuid);

        dispatchEvent(event);
    }

}
