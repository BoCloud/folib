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
package com.folib.components.cron;


import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.services.CronTaskConfigurationService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * @author veadan
 * @date 2024/12/18
 **/
@Slf4j
@Component
public class CronComponent {

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    public void configCronTask(String cronName, String className, String cron) {
        configCronTask(cronName, className, cron, null);
    }

    public void configCronTask(String cronName, String className, String cron, Map<String, String> properties) {
        CronTaskConfigurationDto cronTaskConfiguration = new CronTaskConfigurationDto();
        cronTaskConfiguration.setName(cronName);
        cronTaskConfiguration.setJobClass(className);
        cronTaskConfiguration.setCronExpression(cron);
        cronTaskConfiguration.setOneTimeExecution(false);
        cronTaskConfiguration.setImmediateExecution(false);
        if (MapUtils.isNotEmpty(properties)) {
            cronTaskConfiguration.setProperties(properties);
        }
        try {
            deleteTask(cronTaskConfiguration);
            UUID uuid = cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);
            cronTaskConfiguration.setUuid(uuid);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    public void deleteCronTask(String cronName, String className, String cron) {
        try {
            CronTaskConfigurationDto cronTaskConfiguration = new CronTaskConfigurationDto();
            cronTaskConfiguration.setName(cronName);
            cronTaskConfiguration.setJobClass(className);
            cronTaskConfiguration.setCronExpression(cron);
            cronTaskConfiguration.setOneTimeExecution(false);
            cronTaskConfiguration.setImmediateExecution(false);
            deleteTask(cronTaskConfiguration);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    private void deleteTask(CronTaskConfigurationDto cronTaskConfiguration) throws Exception {
        Optional<CronTaskConfigurationDto> cronTaskConfigurationOptional = cronTaskConfigurationService.getTasksConfigurationDto().getCronTaskConfigurations().stream().filter(item -> item.getJobClass().equals(cronTaskConfiguration.getJobClass()) && item.getName().equals(cronTaskConfiguration.getName())).findFirst();
        if (cronTaskConfigurationOptional.isPresent()) {
            CronTaskConfigurationDto cronTaskConfigurationDto = cronTaskConfigurationOptional.get();
            cronTaskConfigurationService.deleteConfiguration(cronTaskConfigurationDto.getUuid());
        }
    }

}
