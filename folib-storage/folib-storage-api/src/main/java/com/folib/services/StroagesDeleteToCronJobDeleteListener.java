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

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.job.cron.services.CronTaskConfigurationService;
import com.folib.event.repository.RepositoryEvent;
import com.folib.event.repository.RepositoryEventTypeEnum;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.util.Set;

@Component
public class StroagesDeleteToCronJobDeleteListener {
    private static final Logger logger = LoggerFactory.getLogger(StroagesDeleteToCronJobDeleteListener.class);

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    @EventListener
    public void handle(RepositoryEvent event) {
        if (event.getType() != RepositoryEventTypeEnum.EVENT_REPOSITORY_DELETE_ALL_TO_CRON_JOB_DELETED.getType()) {
            return;
        }
        CronTasksConfigurationDto cronTasksConfigurationDto = cronTaskConfigurationService.getTasksConfigurationDto();
        Set<CronTaskConfigurationDto> cronTaskConfigurations = cronTasksConfigurationDto.getCronTaskConfigurations();
        for (CronTaskConfigurationDto configurationDto : cronTaskConfigurations) {
            try {
                String storageId = configurationDto.getProperty("storageId");
                boolean flag = StringUtils.isNotBlank(storageId)
                        && event.getStorageId().equals(storageId);
                if (flag) {
                    cronTaskConfigurationService.deleteConfiguration(configurationDto.getUuid());
                    logger.info("[{}] handle stroages delete delete to cron job delete end", storageId);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}
