package com.veadan.folib.job.cron.services;

import com.veadan.folib.job.cron.services.support.CronTaskConfigurationSearchCriteria;
import com.veadan.folib.job.cron.domain.CronTaskConfiguration;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.domain.CronTasksConfigurationDto;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

/**
 * @author Yougeshwar
 * @author Veadan
 */
public interface CronTaskDataService
{

    CronTaskConfigurationDto getTaskConfigurationDto(UUID cronTaskConfigurationUuid);

    CronTasksConfigurationDto getTasksConfigurationDto();

    List<CronTaskConfiguration> findMatching(CronTaskConfigurationSearchCriteria searchCriteria);

    UUID save(CronTaskConfigurationDto configuration) throws IOException;

    void delete(UUID cronTaskConfigurationUuid) throws IOException;


}
