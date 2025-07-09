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

import com.google.common.collect.ImmutableSet;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.job.cron.jobs.fields.CronJobField;
import com.folib.job.cron.jobs.fields.CronJobNamedField;
import com.folib.job.cron.jobs.fields.CronJobOptionalField;
import com.folib.job.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.folib.job.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.folib.job.cron.jobs.fields.CronJobStringTypeField;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.CocoapodsIndexService;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Inject;
import java.util.Set;

/**
 * 通过代理远程仓库索引定时任务
 * @author veadan
 * @date 2023/9/21 13:30
 */
public class SyncProxyRepositoryIndexCronJob extends JavaCronJob
{
    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Inject
    private CocoapodsIndexService cocoapodsIndexService;
    
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ConfigurationManager configurationManager;

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));
    
    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable 
    {
        logger.info("定时任务，执行定时任务（{}）开始", this.getCronJobDefinition().getName());
        final String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        final String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        final Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (null == repository)
        {
            logger.info("仓库（{}）不存在，无法执行同步远程仓库索引定时任务", String.format("%s:%s", storageId, repositoryId));
            return;
        }
        if (!repository.getType().equals(RepositoryTypeEnum.PROXY.getType()))
        {
            logger.info("当前仓库非代理仓库（{}），无法执行同步远程仓库索引定时任务", String.format("%s:%s", storageId, repositoryId));
            return;
        }
        if (cocoapodsIndexService.getSyncProxyIndexLock(repository))
        {
            logger.info("定时任务，已存在正在执行的同步远程仓库（{}）任务， 跳过当次定时任务执行", String.format("%s:%s", storageId, repositoryId));
            return;
        }
        final boolean syncProxyIndexResult = cocoapodsIndexService.syncProxyIndex(repository);
        logger.info("定时任务，同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId),syncProxyIndexResult?"成功":"失败");
        logger.info("定时任务，执行定时任务（{}）结束", this.getCronJobDefinition().getName());
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(SyncProxyRepositoryIndexCronJob.class.getName())
                .name("定时同步Cocoapods远程仓库索引的任务").scope(COCOAPODS)
                .description("用于定时同步Cocoapods远程仓库索引文件")
                .fields(FIELDS)
                .build();
    }
}
