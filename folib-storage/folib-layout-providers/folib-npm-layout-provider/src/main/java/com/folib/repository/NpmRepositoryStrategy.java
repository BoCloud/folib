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
import com.folib.job.tasks.FetchRemoteNpmChangesFeedCronJob;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

@Component
public class NpmRepositoryStrategy
        extends AbstractRepositoryStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(NpmRepositoryStrategy.class);

    @Lazy
    @Inject
    private CronTaskDataService cronTaskDataService;

//    @Override
//    protected void createRepositoryInternal(Storage storage,
//                                            Repository repository)
//        throws RepositoryManagementStrategyException
//    {
//        String storageId = storage.getId();
//        String repositoryId = repository.getId();
//
//        if (repository.isProxyRepository())
//        {
////            createRemoteChangesFeedFetcherCronTask(storageId, repositoryId);
//        }
//    }

    private void createRemoteChangesFeedFetcherCronTask(String storageId,
                                                        String repositoryId)
            throws RepositoryManagementStrategyException
    {
        CronTaskConfigurationDto configuration = new CronTaskConfigurationDto();
        configuration.setName(FetchRemoteNpmChangesFeedCronJob.calculateJobName(storageId, repositoryId));
        configuration.setJobClass(FetchRemoteNpmChangesFeedCronJob.class.getName());
        configuration.setCronExpression("0 0 * ? * * *"); // Execute every hour
        configuration.addProperty("storageId", storageId);
        configuration.addProperty("repositoryId", repositoryId);
        configuration.setImmediateExecution(true);

        try
        {
            cronTaskDataService.save(configuration);
        }
        catch (Exception e)
        {
            logger.error(e.getMessage(), e);

            throw new RepositoryManagementStrategyException(e.getMessage(), e);
        }
    }

}
