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
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.storage.indexing.RepositoryIndexCreator;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class RebuildMavenIndexesCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.HOSTED)
    private RepositoryIndexCreator repositoryIndexCreator;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        logger.info("Executing RebuildMavenIndexesCronJob for storageId = [{}], repositoryId = [{}]",
                     storageId, repositoryId);

        Repository repository = configurationManager.getRepository(storageId, repositoryId);

        if (!repository.isHostedRepository())
        {
            logger.warn("Repository identified by storageId = [{}], repositoryId = [{}] is not a hosted repository. Exiting ...",
                        storageId, repositoryId);
            return;
        }

        repositoryIndexCreator.apply(repository);
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RebuildMavenIndexesCronJob.class.getName())
                                .name("定时重建Maven仓库的索引任务").scope(MAVEN)
                                .description("该任务用于定时重建Maven仓库的索引")
                                .fields(FIELDS)
                                .build();
    }

}
