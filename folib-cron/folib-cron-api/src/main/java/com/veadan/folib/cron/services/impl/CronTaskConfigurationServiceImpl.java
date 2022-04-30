package com.veadan.folib.cron.services.impl;

import com.veadan.folib.event.cron.CronTaskEventListenerRegistry;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.cron.services.CronJobSchedulerService;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.cron.services.CronTaskDataService;

import javax.inject.Inject;

import java.io.IOException;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.stereotype.Service;

/**
 * @author Pablo Tirado
 */
@Service
class CronTaskConfigurationServiceImpl
        implements CronTaskConfigurationService, ApplicationListener<ContextStartedEvent>
{

    private final Logger logger = LoggerFactory.getLogger(CronTaskConfigurationServiceImpl.class);

    @Inject
    protected CronTaskEventListenerRegistry cronTaskEventListenerRegistry;

    @Inject
    private CronTaskDataService cronTaskDataService;

    @Inject
    private CronJobSchedulerService cronJobSchedulerService;

    @Override
    public void onApplicationEvent(ContextStartedEvent event)
    {
        CronTasksConfigurationDto cronTasksConfiguration = getTasksConfigurationDto();

        for (CronTaskConfigurationDto dto : cronTasksConfiguration.getCronTaskConfigurations())
        {
            if (dto.isOneTimeExecution())
            {
                continue;
            }

            cronJobSchedulerService.scheduleJob(dto);
        }
    }

    public UUID saveConfiguration(CronTaskConfigurationDto configuration) throws IOException
    {
        logger.debug("CronTaskConfigurationService.saveConfiguration()");

        UUID configurationId = cronTaskDataService.save(configuration);
        cronJobSchedulerService.scheduleJob(configuration);

        cronTaskEventListenerRegistry.dispatchCronTaskCreatedEvent(configuration.getUuid());

        return configurationId;
    }

    public void deleteConfiguration(UUID cronTaskConfigurationUuid) throws IOException
    {
        logger.debug("Deleting cron task configuration {}", cronTaskConfigurationUuid);

        cronTaskDataService.delete(cronTaskConfigurationUuid);
        cronJobSchedulerService.deleteJob(cronTaskConfigurationUuid);

        cronTaskEventListenerRegistry.dispatchCronTaskDeletedEvent(cronTaskConfigurationUuid);
    }

    @Override
    public CronTaskConfigurationDto getTaskConfigurationDto(UUID uuid)
    {
        return cronTaskDataService.getTaskConfigurationDto(uuid);
    }

    public CronTasksConfigurationDto getTasksConfigurationDto()
    {
        return cronTaskDataService.getTasksConfigurationDto();
    }

}
