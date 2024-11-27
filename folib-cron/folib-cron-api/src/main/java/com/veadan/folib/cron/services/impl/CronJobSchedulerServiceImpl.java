package com.veadan.folib.cron.services.impl;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.GroovyScriptNamesDto;
import com.veadan.folib.cron.jobs.GroovyCronJob;
import com.veadan.folib.cron.services.CronJobSchedulerService;

import javax.inject.Inject;
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
