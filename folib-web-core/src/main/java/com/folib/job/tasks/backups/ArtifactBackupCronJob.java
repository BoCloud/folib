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
package com.folib.job.tasks.backups;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.components.backup.BackupComponent;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Map;

/**
 * @author veadan
 **/
@Slf4j
public class ArtifactBackupCronJob extends JavaCronJob {

    public static final String PROPERTY_BACKUP_PATH = "backupPath";

    public static final String PROPERTY_STRATEGY_NAME = "strategyName";

    public static final String PROPERTY_STORAGE_ID = "storageId";

    public static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    public static final String PROPERTY_INCREMENTAL = "incremental";

    @Autowired
    private BackupComponent backupComponent;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        String strategyName = config.getProperty(PROPERTY_STRATEGY_NAME);
        String backupPath = config.getProperty(PROPERTY_BACKUP_PATH);
        String incremental = config.getProperty(PROPERTY_INCREMENTAL);
        if (StringUtils.isBlank(backupPath)) {
            return;
        }
        Map<String, String> properties = config.getProperties();
        Map<String, String> propertiesMap = Maps.newLinkedHashMap(properties);
        propertiesMap.remove(PROPERTY_STRATEGY_NAME);
        propertiesMap.remove(PROPERTY_BACKUP_PATH);
        propertiesMap.remove(PROPERTY_STORAGE_ID);
        propertiesMap.remove(PROPERTY_REPOSITORY_ID);
        propertiesMap.remove(PROPERTY_INCREMENTAL);
        if (MapUtils.isEmpty(propertiesMap)) {
            //全量备份
            return;
        }
        backupComponent.backupRepositories(strategyName, backupPath, Boolean.TRUE.equals(Boolean.valueOf(incremental)), Lists.newLinkedList(propertiesMap.keySet()));
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ArtifactBackupCronJob.class.getName())
                .name("定时全量备份制品的任务")
                .description("定时全量备份制品的任务")
                .build();
    }

}
