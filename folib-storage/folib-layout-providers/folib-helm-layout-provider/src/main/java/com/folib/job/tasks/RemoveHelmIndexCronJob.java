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
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.indexer.HelmMetadataIndexer;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Map;
import java.util.Set;

public class RemoveHelmIndexCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";
    private static final String INDEX_FILE = "index.yaml";
    private static final String CHARTS_INDEX_FILE = "charts/index.yaml";

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ArtifactManagementService artifactManagementService;

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));


    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {

        if (config == null) {
            throw new IllegalArgumentException("Config cannot be null");
        }
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        if (storageId == null || repositoryId == null) {
            return;
        }

        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (repository != null && RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            RepositoryPath trashPath = repositoryPathResolver.resolve(storageId, repositoryId, INDEX_FILE);
            RepositoryPath trashPath2 = repositoryPathResolver.resolve(storageId, repositoryId, CHARTS_INDEX_FILE);
            if (Files.exists(trashPath)) {
                Files.deleteIfExists(trashPath);
                artifactResolutionService.resolvePath(storageId, repositoryId, INDEX_FILE);
            } else if (Files.exists(trashPath2)) {
                Files.deleteIfExists(trashPath2);
                artifactResolutionService.resolvePath(storageId, repositoryId, CHARTS_INDEX_FILE);
            } else {
                RepositoryPath path = artifactResolutionService.resolvePath(storageId, repositoryId, INDEX_FILE);
                if (path == null) {
                    path = artifactResolutionService.resolvePath(storageId, repositoryId, CHARTS_INDEX_FILE);
                }
                if (path == null) {
                   logger.warn("未找到对应的{} {} index.yaml文件",storageId, repositoryId);
                }
            }
        } else if (repository != null && RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            HelmMetadataIndexer indexer = new HelmMetadataIndexer(storageId, repositoryId, artifactManagementService, repositoryPathResolver);
            indexer.reindexAsSystem();
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveHelmIndexCronJob.class.getName())
                .name("定时更新helm的index.yaml任务").scope(HELM)
                .description("该任务用于定时更新helm库的index.yaml")
                .fields(FIELDS)
                .build();
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }
}
