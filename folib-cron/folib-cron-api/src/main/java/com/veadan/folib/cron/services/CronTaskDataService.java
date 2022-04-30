package com.veadan.folib.cron.services;

import com.veadan.folib.cron.services.support.CronTaskConfigurationSearchCriteria;
import com.veadan.folib.cron.domain.CronTaskConfiguration;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

/**
 * @author Yougeshwar
 * @author Pablo Tirado
 */
public interface CronTaskDataService
{

    CronTaskConfigurationDto getTaskConfigurationDto(UUID cronTaskConfigurationUuid);

    CronTasksConfigurationDto getTasksConfigurationDto();

    List<CronTaskConfiguration> findMatching(CronTaskConfigurationSearchCriteria searchCriteria);

    UUID save(CronTaskConfigurationDto configuration) throws IOException;

    void delete(UUID cronTaskConfigurationUuid) throws IOException;


}
