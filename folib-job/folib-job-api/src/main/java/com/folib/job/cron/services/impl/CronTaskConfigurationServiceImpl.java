package com.folib.job.cron.services.impl;

import com.folib.job.event.cron.CronTaskEventListenerRegistry;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.job.cron.services.CronJobSchedulerService;
import com.folib.job.cron.services.CronTaskConfigurationService;
import com.folib.job.cron.services.CronTaskDataService;

import jakarta.inject.Inject;

import java.io.IOException;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.stereotype.Service;

/**
 * @author Veadan
 */
@Service
class CronTaskConfigurationServiceImpl
        implements CronTaskConfigurationService, ApplicationListener<ContextStartedEvent> {

    private final Logger logger = LoggerFactory.getLogger(CronTaskConfigurationServiceImpl.class);

    @Inject
    protected CronTaskEventListenerRegistry cronTaskEventListenerRegistry;

    @Inject
    private CronTaskDataService cronTaskDataService;

    @Inject
    private CronJobSchedulerService cronJobSchedulerService;

    @Override
    public void onApplicationEvent(ContextStartedEvent event) {
        logger.info("---定时任务事件监听---");
        CronTasksConfigurationDto cronTasksConfiguration = getTasksConfigurationDto();

        for (CronTaskConfigurationDto dto : cronTasksConfiguration.getCronTaskConfigurations()) {
            if (dto.isOneTimeExecution()) {
                continue;
            }
            cronJobSchedulerService.scheduleJob(dto, true);
        }
        logger.info("---定时任务已全部启动---");
    }

    @Override
    public UUID saveConfiguration(CronTaskConfigurationDto configuration) throws IOException {
        logger.info("CronTaskConfigurationService.saveConfiguration()");

        UUID configurationId = cronTaskDataService.save(configuration);
        cronJobSchedulerService.scheduleJob(configuration, false);

        cronTaskEventListenerRegistry.dispatchCronTaskCreatedEvent(configuration.getUuid());

        return configurationId;
    }

    @Override
    public void deleteConfiguration(UUID cronTaskConfigurationUuid) throws IOException {
        logger.info("Deleting cron task configuration {}", cronTaskConfigurationUuid);

        cronTaskDataService.delete(cronTaskConfigurationUuid);
        cronJobSchedulerService.deleteJob(cronTaskConfigurationUuid);

        cronTaskEventListenerRegistry.dispatchCronTaskDeletedEvent(cronTaskConfigurationUuid);
    }

    @Override
    public CronTaskConfigurationDto getTaskConfigurationDto(UUID uuid) {
        return cronTaskDataService.getTaskConfigurationDto(uuid);
    }

    @Override
    public CronTasksConfigurationDto getTasksConfigurationDto() {
        return cronTaskDataService.getTasksConfigurationDto();
    }


}
