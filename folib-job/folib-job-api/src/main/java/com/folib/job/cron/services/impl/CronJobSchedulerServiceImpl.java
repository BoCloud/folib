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

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.GroovyScriptNamesDto;
import com.folib.job.cron.jobs.GroovyCronJob;
import com.folib.job.cron.services.CronJobSchedulerService;

import jakarta.inject.Inject;
import java.util.Set;
import java.util.UUID;

import org.quartz.*;
import org.quartz.impl.matchers.GroupMatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * @author Yougeshwar
 * @author Veadan
 */
@Service
public class CronJobSchedulerServiceImpl
        implements CronJobSchedulerService
{

    private static final Logger logger = LoggerFactory.getLogger(CronJobSchedulerServiceImpl.class);

    @Inject
    private Scheduler scheduler;

    @Override
    public void scheduleJob(CronTaskConfigurationDto cronTaskConfiguration, boolean serviceStartup)
    {
        String jobClassName = cronTaskConfiguration.getJobClass();
        Class<? extends Job> jobClass;
        try
        {
            jobClass = (Class<? extends Job>) Class.forName(jobClassName);
        }
        catch (ClassNotFoundException e)
        {
            logger.error("Failed to schedule cron job [{}]", jobClassName, e);

            return;
        }

        // delete old job if exists
        deleteJob(cronTaskConfiguration.getUuid());

        JobDataMap jobDataMap = new JobDataMap();
        jobDataMap.put("config", cronTaskConfiguration);

        JobKey jobKey = JobKey.jobKey(cronTaskConfiguration.getUuid().toString());
        JobDetail jobDetail = JobBuilder.newJob(jobClass)
                                        .withIdentity(jobKey)
                                        .setJobData(jobDataMap)
                                        .storeDurably()
                                        .build();

        try
        {
            scheduler.addJob(jobDetail, true);
            logger.info("Job '{}' added to the Scheduler.", cronTaskConfiguration.getUuid());
        }
        catch (SchedulerException e)
        {
            logger.error("Failed to add Cron Job [{}] to the Scheduler", cronTaskConfiguration, e);
            return;
        }

        boolean scheduleJob = true;

        if (!serviceStartup && cronTaskConfiguration.shouldExecuteImmediately())
        {
            try
            {
                scheduler.triggerJob(jobKey);
                logger.info("Job '{}' triggered by the Scheduler.", cronTaskConfiguration.getUuid());
            }
            catch (SchedulerException e)
            {
                logger.error("Failed to trigger Cron Job [{}] by the Scheduler", cronTaskConfiguration, e);
                return;
            }

            scheduleJob = !cronTaskConfiguration.isOneTimeExecution();
        }

        if (!scheduleJob)
        {
            logger.info("Job '{}' won't be scheduled based on the cron expression.", cronTaskConfiguration.getUuid());
            return;
        }

        TriggerKey triggerKey = TriggerKey.triggerKey(cronTaskConfiguration.getUuid().toString());
        TriggerBuilder<Trigger> triggerBuilder = TriggerBuilder.newTrigger()
                                                               .withIdentity(triggerKey)
                                                               .forJob(jobDetail);

        String cronExpression = cronTaskConfiguration.getCronExpression();
        triggerBuilder.withSchedule(CronScheduleBuilder.cronSchedule(cronExpression));
        Trigger trigger = triggerBuilder.build();

        try
        {
            scheduler.scheduleJob(trigger);
            logger.info("Job '{}' scheduled.", cronTaskConfiguration.getUuid());
        }
        catch (SchedulerException e)
        {
            logger.error("Failed to schedule Cron Job: [{}]", cronTaskConfiguration, e);

            return;
        }
    }

    @Override
    public void deleteJob(UUID cronTaskConfigurationUuid)
    {
        JobKey jobKey = JobKey.jobKey(cronTaskConfigurationUuid.toString());

        try
        {
            scheduler.deleteJob(jobKey);
            logger.info("Job '{}' deleted and un-scheduled.", cronTaskConfigurationUuid);
        }
        catch (SchedulerException e)
        {
            logger.error("Failed to delete cron job [{}]", jobKey);
        }
    }

    @Override
    public GroovyScriptNamesDto getGroovyScriptsName()
    {
        GroovyScriptNamesDto groovyScriptNames = new GroovyScriptNamesDto();

        Set<JobKey> jobKeySet;
        try
        {
            jobKeySet = scheduler.getJobKeys(GroupMatcher.anyJobGroup());
        }
        catch (SchedulerException e)
        {
            return groovyScriptNames;
        }

        for (JobKey jobKey : jobKeySet)
        {
            JobDetail jobDetail;

            try
            {
                jobDetail = scheduler.getJobDetail(jobKey);
            }
            catch (SchedulerException e)
            {
                continue;
            }

            JobDataMap jobDataMap = jobDetail.getJobDataMap();

            CronTaskConfigurationDto configuration = (CronTaskConfigurationDto) jobDataMap.get("config");

            if (!GroovyCronJob.class.getName().equals(configuration.getJobClass()))
            {
                continue;
            }

            String groovyScriptName = configuration.getProperties().get("fileName");
            groovyScriptNames.addName(groovyScriptName);
        }

        return groovyScriptNames;
    }

}
