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
import java.io.IOException;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.springframework.core.env.Environment;

/**
 * @author Kate Novik
 * @author Veadan
 */
public class DownloadRemoteMavenIndexCronJob
        extends JavaCronJob
{

    public static final String FOLIB_DOWNLOAD_INDEXES = "folib.download.indexes";

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    @RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.PROXY)
    private RepositoryIndexCreator repositoryIndexCreator;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws IOException
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        logger.info("Executing DownloadRemoteMavenIndexCronJob for storageId = [{}], repositoryId = [{}]",
                     storageId, repositoryId);

        Repository repository = configurationManager.getRepository(storageId, repositoryId);

        if (!repository.isProxyRepository())
        {
            logger.warn("Repository identified by storageId = [{}], repositoryId = [{}] is not a proxy repository. Exiting ...",
                        storageId, repositoryId);
            return;
        }

        repositoryIndexCreator.apply(repository);
    }

    @Override
    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env)
    {
        if (!super.enabled(configuration, env))
        {
            return false;
        }

        String storageId = configuration.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = configuration.getProperty(PROPERTY_REPOSITORY_ID);

        boolean shouldDownloadIndexes = shouldDownloadAllRemoteRepositoryIndexes();
        boolean shouldDownloadRepositoryIndex = shouldDownloadRepositoryIndex(storageId, repositoryId);

        return shouldDownloadIndexes || shouldDownloadRepositoryIndex;
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(DownloadRemoteMavenIndexCronJob.class.getName())
                                .name("定时拉取远程Maven仓库索引").scope(MAVEN)
                                .description("该任务用于定时拉取远程仓库下的索引文件")
                                .fields(FIELDS)
                                .build();
    }

    public static boolean shouldDownloadAllRemoteRepositoryIndexes()
    {
        return System.getProperty(FOLIB_DOWNLOAD_INDEXES) == null ||
               Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES));
    }

    public static boolean shouldDownloadRepositoryIndex(String storageId,
                                                        String repositoryId)
    {
        return (System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "." + repositoryId) == null ||
                Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "."
                                                        + repositoryId)))
               &&
               isIncludedDespiteWildcard(storageId, repositoryId);
    }

    public static boolean isIncludedDespiteWildcard(String storageId,
                                                    String repositoryId)
    {
        return // is excluded by wildcard
                !Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + ".*")) &&
                // and is explicitly included
                Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "."
                                                        + repositoryId));
    }

}
