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
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.metadata.indexer.RpmRepoIndexer;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 * @date 2024/12/10
 **/
@Slf4j
public class RebuildRpmMetadataCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private ConfigurationManager configurationManager;

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        Storage storage = configurationManager.getStorage(storageId);
        if (Objects.isNull(storage)) {
            log.warn("Storage [{}] not found", storageId);
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            log.warn("Storage [{}] repository [{}] not found", storageId, repositoryId);
            return;
        }
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            log.warn("Repository storageId [{}] repositoryId [{}] not is hosted type skip..", storageId, repositoryId);
            return;
        }
        RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
        rpmRepoIndexer.indexWriter(repository);
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RebuildRpmMetadataCronJob.class.getName())
                .name("定时重建Rpm仓库的索引任务").scope(RPM)
                .description("该任务用于定时重建Rpm仓库制品包的索引任务")
                .fields(FIELDS)
                .build();
    }
}

