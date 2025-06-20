package com.veadan.folib.job.cron.services;

import com.veadan.folib.job.cron.exceptions.CronTaskNotFoundException;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.domain.CronTasksConfigurationDto;

import java.io.IOException;
import java.util.UUID;

import org.quartz.SchedulerException;

/**
 * @author Veadan
 */
public interface CronTaskConfigurationService
{


    UUID saveConfiguration(CronTaskConfigurationDto cronTaskConfiguration)
            throws Exception;

    void deleteConfiguration(UUID cronTaskConfigurationUuid)
            throws SchedulerException,
            CronTaskNotFoundException,
                   ClassNotFoundException, IOException;

    CronTaskConfigurationDto getTaskConfigurationDto(UUID cronTaskConfigurationUuid);

    CronTasksConfigurationDto getTasksConfigurationDto();

}
