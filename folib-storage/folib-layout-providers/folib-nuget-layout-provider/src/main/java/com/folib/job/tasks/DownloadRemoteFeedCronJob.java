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

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.job.cron.jobs.fields.*;
import com.folib.repository.NugetRepositoryFeatures;

import javax.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.springframework.core.env.Environment;

/**
 * @author veadan
 */
public class DownloadRemoteFeedCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private NugetRepositoryFeatures features;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        features.downloadRemoteFeed(storageId, repositoryId);
    }

    @Override
    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env)
    {
        if (!super.enabled(configuration, env))
        {
            return false;
        }

        return shouldDownloadRemoteRepositoryFeed();
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(DownloadRemoteFeedCronJob.class.getName())
                                .name("定时拉取远程NUGET仓库Feed记录的任务").scope(NUGET)
                                .description("用于定时拉取远程NUGET仓库Feed记录的任务")
                                .fields(FIELDS)
                                .build();
    }


    public static boolean shouldDownloadRemoteRepositoryFeed()
    {
        return System.getProperty("folib.nuget.download.feed") == null ||
               Boolean.parseBoolean(System.getProperty("folib.nuget.download.feed"));
    }
}
