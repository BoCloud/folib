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
package com.folib.job.tasks;

import com.folib.job.cron.jobs.fields.*;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.services.ChecksumService;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections4.CollectionUtils;

/**
 * @author veadan
 */
public class RegenerateChecksumCronTask
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_FORCE_REGENERATION = "forceRegeneration";

    private static final String PROPERTY_LAST_MODIFIED_TIME = "lastModifiedTime";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobBooleanTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_FORCE_REGENERATION), "是否强制覆盖"))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_BASE_PATH), "仓库相对路径"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_LAST_MODIFIED_TIME), "几天内的制品"))));

    @Inject
    private ChecksumService checksumService;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);
        String lastModifiedTime = config.getProperty(PROPERTY_LAST_MODIFIED_TIME);
        /**
         * The values of forceRegeneration are:
         * - true  - to re-write existing checksum and to regenerate missing checksum,
         * - false - to regenerate missing checksum only
         */
        boolean forceRegeneration = Boolean.valueOf(config.getProperty(PROPERTY_FORCE_REGENERATION));

        if (storageId == null && repositoryId == null) {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet()) {
                regenerateRepositoriesChecksum(storage, lastModifiedTime, forceRegeneration);
            }
        } else if (storageId != null && repositoryId != null) {
            checksumService.regenerateChecksum(storageId, repositoryId, basePath, lastModifiedTime, forceRegeneration);
        } else {
            throw new IllegalArgumentException("storageId and repositoryId can not be null");
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RegenerateChecksumCronTask.class.getName())
                                .name("定时重新生成制品的Checksum文件")
                                .scope(GLOBAL)
                                .description("该任务用于重新生成制品的Checksum文件")
                                .fields(FIELDS)
                                .build();
    }

    /**
     * To regenerate artifact's checksum in repositories
     *
     * @param storageId         path of storage
     * @param lastModifiedTime  处理多少天内的制品
     * @param forceRegeneration true - to re-write existing checksum and to regenerate missing checksum,
     *                          false - to regenerate missing checksum only
     * @throws IOException
     */
    private void regenerateRepositoriesChecksum(String storageId, String lastModifiedTime,
                                                boolean forceRegeneration)
            throws IOException
    {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        for (String repositoryId : repositories.keySet())
        {
            if(!existsRepositoryTask(storageId, repositoryId)){
                checksumService.regenerateChecksum(storageId, repositoryId, null, lastModifiedTime, forceRegeneration);
            }
        }
    }

    private Map<String, Storage> getStorages()
    {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId)
    {
        return getStorages().get(storageId).getRepositories();
    }

    private boolean existsRepositoryTask(String storageId, String repositoryId) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        String cronJob = "com.folib.cron.jobs.RegenerateChecksumCronJob";
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && cronJob.equals(cron.getJobClass()));
    }

}
