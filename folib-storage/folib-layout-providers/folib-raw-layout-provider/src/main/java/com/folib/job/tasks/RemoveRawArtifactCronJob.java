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
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.repository.RawRepositoryFeatures;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;

import javax.inject.Inject;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 */
public class RemoveRawArtifactCronJob
        extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_NUMBER_TO_KEEP = "numberToKeep";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_NUMBER_TO_KEEP), "保留版本"))));

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RawRepositoryFeatures rawRepositoryFeatures;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);
        // The number of artifacts to keep
        int numberToKeep = config.getProperty(PROPERTY_NUMBER_TO_KEEP) != null ?
                Integer.parseInt(config.getProperty(PROPERTY_NUMBER_TO_KEEP)) :
                0;
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
        rawRepositoryFeatures.removeRawArtifact(storageId,
                repositoryId,
                basePath,
                numberToKeep, cleanupArtifactPathMap);
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveRawArtifactCronJob.class.getName())
                .name("定时删除Raw制品任务").scope(RAW)
                .description("该任务用于按照版本数量清理Raw制品包，启用该任务需使用自定义布局")
                .fields(FIELDS)
                .build();
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId) {
        return getStorages().get(storageId).getRepositories();
    }

}
