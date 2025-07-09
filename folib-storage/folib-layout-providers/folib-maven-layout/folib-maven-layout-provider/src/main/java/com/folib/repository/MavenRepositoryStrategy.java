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
package com.folib.repository;

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.services.CronTaskDataService;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.job.tasks.DownloadRemoteMavenIndexCronJob;
import com.folib.job.tasks.MergeMavenGroupRepositoryIndexCronJob;
import com.folib.job.tasks.RebuildMavenIndexesCronJob;
import com.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class MavenRepositoryStrategy
        extends AbstractRepositoryStrategy
{
    @Lazy
    @Inject
    private CronTaskDataService cronTaskDataService;
    @Lazy
    @Inject
    private MavenRepositoryFeatures repositoryFeatures;

    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

//    @Override
//    protected void createRepositoryInternal(Storage storage,
//                                            Repository repository)
//            throws RepositoryManagementStrategyException
//    {
//        if (!repositoryFeatures.isIndexingEnabled(repository))
//        {
//            return;
//        }
//
//        String storageId = storage.getId();
//        String repositoryId = repository.getId();
//        MavenRepositoryConfiguration repositoryConfig =
//                (MavenRepositoryConfiguration) repository.getRepositoryConfiguration();
//
//        if (repository.isHostedRepository())
//        {
//            createRebuildMavenIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//        if (repository.isProxyRepository())
//        {
//            createRemoteIndexDownloaderCronTask(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//        if (repository.isGroupRepository())
//        {
//            createMergeMavenGroupRepositoryIndexCronJob(storageId, repositoryId, repositoryConfig.getCronExpression());
//        }
//    }

    private void createRemoteIndexDownloaderCronTask(String storageId,
                                                     String repositoryId,
                                                     String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Remote index download for " + storageId + ":" + repositoryId);
        configuration.setJobClass(DownloadRemoteMavenIndexCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(true);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    private void createRebuildMavenIndexCronJob(String storageId,
                                                String repositoryId,
                                                String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Rebuild Maven Index Cron Job for " + storageId + ":" + repositoryId);
        configuration.setJobClass(RebuildMavenIndexesCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(true);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    private void createMergeMavenGroupRepositoryIndexCronJob(String storageId,
                                                             String repositoryId,
                                                             String cronExpression)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName("Merge maven group repository index cron job " + storageId + ":" + repositoryId);
        configuration.setJobClass(MergeMavenGroupRepositoryIndexCronJob.class.getName());
        configuration.setCronExpression(cronExpression);
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(false);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

    @Override
    public void createRepositoryStructure(final Repository repository)
            throws IOException
    {
        super.createRepositoryStructure(repository);

        final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        final RepositoryPath indexRepositoryPath = rootRepositoryPath.resolve(MavenRepositoryFeatures.INDEX);
        if (!Files.exists(indexRepositoryPath))
        {
            Files.createDirectories(indexRepositoryPath);
        }
    }

}
