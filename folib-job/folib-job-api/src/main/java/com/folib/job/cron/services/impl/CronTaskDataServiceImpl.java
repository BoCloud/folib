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
package com.folib.job.cron.services.impl;

import com.folib.job.cron.services.support.CronTaskConfigurationSearchCriteria;
import com.folib.job.cron.config.CronTasksConfigurationFileManager;
import com.folib.job.cron.domain.CronTaskConfiguration;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.job.cron.jobs.CronJobDuplicationCheckStrategy;
import com.folib.job.cron.jobs.CronJobsDefinitionsRegistry;
import com.folib.job.cron.jobs.CronJobDuplicationCheckStrategiesRegistry;
import com.folib.job.cron.services.CronTaskDataService;

import jakarta.inject.Inject;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * @author Yougeshwar
 * @author Veadan
 * @author veadan
 */
@Service
public class CronTaskDataServiceImpl
        implements CronTaskDataService
{

    private static final Logger logger = LoggerFactory.getLogger(CronTaskDataService.class);

    private final ReadWriteLock cronTasksConfigurationLock = new ReentrantReadWriteLock();

    private CronTasksConfigurationFileManager cronTasksConfigurationFileManager;

    private CronJobsDefinitionsRegistry cronJobsDefinitionsRegistry;

    private CronJobDuplicationCheckStrategiesRegistry cronJobDuplicationCheckStrategiesRegistry;

    /**
     * Yes, this is a state object.
     * It is protected by the {@link #cronTasksConfigurationLock} here
     * and should not be exposed to the world.
     */
    private final CronTasksConfigurationDto configuration;

    @Lazy
    @Inject
    CronTaskDataServiceImpl(CronTasksConfigurationFileManager cronTasksConfigurationFileManager,
                            CronJobsDefinitionsRegistry cronJobsDefinitionsRegistry,
                            CronJobDuplicationCheckStrategiesRegistry cronJobDuplicationCheckStrategiesRegistry)
            throws IOException
    {
        this.cronTasksConfigurationFileManager = cronTasksConfigurationFileManager;
        this.cronJobsDefinitionsRegistry = cronJobsDefinitionsRegistry;
        this.cronJobDuplicationCheckStrategiesRegistry = cronJobDuplicationCheckStrategiesRegistry;

        CronTasksConfigurationDto cronTasksConfiguration = cronTasksConfigurationFileManager.read();
        for (Iterator<CronTaskConfigurationDto> iterator = cronTasksConfiguration.getCronTaskConfigurations().iterator(); iterator.hasNext(); )
        {
            CronTaskConfigurationDto c = iterator.next();

            logger.info("Saving cron configuration {}", c);

            String jobClass = c.getJobClass();
            if (jobClass != null && !jobClass.trim().isEmpty())
            {
                try
                {
                    Class.forName(jobClass);
                }
                catch (ClassNotFoundException e)
                {
                    logger.warn("Skip configuration, job class not found [{}].", jobClass);
                    iterator.remove();
                }
            }

        }

        this.configuration = cronTasksConfiguration;
    }

    @Override
    public CronTasksConfigurationDto getTasksConfigurationDto()
    {
        final Lock readLock = cronTasksConfigurationLock.readLock();
        readLock.lock();

        try
        {
            return SerializationUtils.clone(configuration);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public CronTaskConfigurationDto getTaskConfigurationDto(UUID cronTaskConfigurationUuid)
    {
        final Lock readLock = cronTasksConfigurationLock.readLock();
        readLock.lock();

        try
        {
            Optional<CronTaskConfigurationDto> cronTaskConfiguration = configuration.getCronTaskConfigurations()
                                                                                    .stream()
                                                                                    .filter(conf -> Objects.equals(
                                                                                            cronTaskConfigurationUuid,
                                                                                            conf.getUuid()))
                                                                                    .findFirst();
            return cronTaskConfiguration.map(SerializationUtils::clone).orElse(null);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public List<CronTaskConfiguration> findMatching(CronTaskConfigurationSearchCriteria searchCriteria)
    {
        final Lock readLock = cronTasksConfigurationLock.readLock();
        readLock.lock();

        try
        {
            Stream<CronTaskConfigurationDto> stream = configuration.getCronTaskConfigurations().stream();
            if (!searchCriteria.isEmpty())
            {
                if (!CollectionUtils.isEmpty(searchCriteria.getProperties()))
                {
                    for (Map.Entry<String, String> entry : searchCriteria.getProperties().entrySet())
                    {
                        stream = stream.filter(conf -> entry.getValue()
                                                            .equals(conf.getProperties().get(entry.getKey())));
                    }
                }
            }
            return stream.map(CronTaskConfiguration::new).collect(Collectors.toList());
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public UUID save(final CronTaskConfigurationDto dto)
            throws IOException
    {
        setUuidIfNull(dto);
        setNameIfBlank(dto);

        modifyInLock(configuration ->
                     {
                         if (isDuplicate(dto, configuration))
                         {
                             return;
                         }
                         removePreviousIncarnation(dto, configuration);
                         configuration.getCronTaskConfigurations().add(dto);
                     });

        return dto.getUuid();
    }

    @Override
    public void delete(final UUID cronTaskConfigurationUuid)
            throws IOException
    {
        modifyInLock(configuration ->
                             configuration.getCronTaskConfigurations()
                                          .stream()
                                          .filter(conf -> Objects.equals(cronTaskConfigurationUuid, conf.getUuid()))
                                          .findFirst()
                                          .ifPresent(conf -> configuration.getCronTaskConfigurations().remove(conf)));
    }

    private void modifyInLock(final Consumer<CronTasksConfigurationDto> operation)
            throws IOException
    {
        modifyInLock(operation, true);
    }

    private void modifyInLock(final Consumer<CronTasksConfigurationDto> operation,
                              final boolean storeInFile)
            throws IOException
    {
        final Lock writeLock = cronTasksConfigurationLock.writeLock();
        writeLock.lock();

        try
        {
            operation.accept(configuration);

            if (storeInFile)
            {
                cronTasksConfigurationFileManager.store(configuration);
            }
        }
        finally
        {
            writeLock.unlock();
        }
    }

    private void removePreviousIncarnation(final CronTaskConfigurationDto dto,
                                           final CronTasksConfigurationDto configuration)
    {
        if (configuration.getCronTaskConfigurations() == null)
        {
            return;
        }
        for (final Iterator<CronTaskConfigurationDto> it = configuration.getCronTaskConfigurations().iterator(); it.hasNext(); )
        {
            final CronTaskConfigurationDto existing = it.next();
            if (Objects.equals(dto.getUuid(), existing.getUuid()))
            {
                it.remove();
            }
        }
    }

    private void setUuidIfNull(final CronTaskConfigurationDto dto)
    {
        if (dto.getUuid() == null)
        {
            dto.setUuid(UUID.randomUUID());
        }
    }

    private void setNameIfBlank(final CronTaskConfigurationDto dto)
    {
        if (StringUtils.isBlank(dto.getName()))
        {
            cronJobsDefinitionsRegistry.getCronJobDefinitions()
                                       .stream()
                                       .filter(cj -> Objects.equals(cj.getJobClass(), dto.getJobClass()))
                                       .findFirst()
                                       .map(cj -> {
                                           dto.setName(cj.getName());
                                           return cj;
                                       }).orElseThrow(
                    () -> new IllegalArgumentException(String.format("Unrecognized cron job %s", dto.getJobClass())));
        }
    }

    private boolean isDuplicate(final CronTaskConfigurationDto dto,
                                final CronTasksConfigurationDto configuration)
    {
        final Set<CronJobDuplicationCheckStrategy> cronJobDuplicationStrategies = cronJobDuplicationCheckStrategiesRegistry.get(
                dto.getJobClass());
        return cronJobDuplicationStrategies.stream()
                                           .anyMatch(s -> s.duplicates(dto, configuration.getCronTaskConfigurations()));
    }

}
