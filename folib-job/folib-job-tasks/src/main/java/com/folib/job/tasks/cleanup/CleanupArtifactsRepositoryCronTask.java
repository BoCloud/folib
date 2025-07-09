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
package com.folib.job.tasks.cleanup;

import com.folib.job.cron.jobs.fields.*;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;

import jakarta.inject.Inject;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * 清理保存N 天前的制品
 *
 * @author veadan
 * @date 2023/02/01
 */
@Slf4j
public class CleanupArtifactsRepositoryCronTask extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_STORAGE_DAY = "storageDay";

    private static final String PROPERTY_STORAGE_CONDITION = "storageCondition";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_STORAGE_CONDITION), "保留条件"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_STORAGE_DAY), "保留天数"))));

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String storageDay = config.getProperty(PROPERTY_STORAGE_DAY);
        String storageCondition = config.getProperty(PROPERTY_STORAGE_CONDITION);
        Map<String, String> properties = config.getProperties();
        String artifactPathPrefix = "artifactPath:";
        Map<String, String> cleanupArtifactPathMap = Maps.newLinkedHashMap();
        for (Map.Entry<String, String> entry : properties.entrySet()) {
            String key = entry.getKey();
            if (!key.startsWith(artifactPathPrefix)) {
                continue;
            }
            key = key.replace(artifactPathPrefix, "");
            if (StringUtils.isBlank(key) || StringUtils.isBlank(entry.getValue())) {
                continue;
            }
            cleanupArtifactPathMap.put(key, entry.getValue());
        }

        if (storageId == null && repositoryId == null) {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet()) {
                handleCleanup(storage, storageDay, storageCondition, cleanupArtifactPathMap);
            }
        } else if(storageId != null && repositoryId != null) {
            regenerateRepositoriesCleanup(storageId, repositoryId, storageDay, storageCondition, cleanupArtifactPathMap);
        }else {
            throw new IllegalArgumentException("storageId and repositoryId can not be null");
        }

    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(CleanupArtifactsRepositoryCronTask.class.getName())
                .name("仓库自定义清理任务")
                .scope(GLOBAL)
                .description("该任务可定时删除制品仓库下的制品文件")
                .fields(FIELDS)
                .build();
    }

    public void handleCleanup(String storageId,  String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);
        for (String repositoryId : repositories.keySet()) {
            if (!existsRepositoryTask(storageId, repositoryId)) {
                regenerateRepositoriesCleanup(storageId, repositoryId, storageDay, storageCondition, cleanupArtifactPathMap);
            }
        }
    }

    public void regenerateRepositoriesCleanup(String storageId, String repositoryId, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception {
        log.info("Start clean artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]", storageId, repositoryId, storageCondition, storageDay);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && StringUtils.isNotBlank(storageDay)) {
            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            if (Objects.isNull(repository)) {
                log.warn("Repository storageId [{}] repositoryId [{}] not found", storageId, repositoryId);
                return;
            }
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                log.warn("Repository storageId [{}] repositoryId [{}] is group type skip..", storageId, repositoryId);
                return;
            }
            if (!NumberUtils.isDigits(storageDay) || Integer.parseInt(storageDay) <= 0) {
                log.warn("Params storageDay [{}] error", storageDay);
                return;
            }
            String dockerLayout = "Docker", cleanupRepositoryType = "GENERAL";
            if (dockerLayout.equalsIgnoreCase(repository.getLayout())) {
                cleanupRepositoryType = "DOCKER";
            }
            CleanupArtifactsProvider cleanupArtifactsProvider = cleanupArtifactsProviderRegistry.getProvider(cleanupRepositoryType);
            cleanupArtifactsProvider.cleanupV2(storageId, repositoryId, "", storageDay, storageCondition, cleanupArtifactPathMap);
        } else {
            log.warn("Repository storageId repositoryId storageDay should not be null");
        }
        log.info("Clean artifact job end storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]", storageId, repositoryId, storageCondition, storageDay);
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId) {
        return getStorages().get(storageId).getRepositories();
    }
    private boolean existsRepositoryTask(String storageId, String repositoryId) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        String cronJob = "com.folib.cron.jobs.cleanup.CleanupArtifactsRepositoryCronJob";
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && cronJob.equals(cron.getJobClass()));
    }
}