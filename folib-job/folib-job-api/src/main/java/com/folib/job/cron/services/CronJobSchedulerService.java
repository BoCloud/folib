package com.folib.job.cron.services;

import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.GroovyScriptNamesDto;

import java.util.UUID;

/**
 * @author Veadan
 * @author Veadan
 */
public interface CronJobSchedulerService
{

    void scheduleJob(CronTaskConfigurationDto cronTaskConfiguration, boolean serviceStartup);

    void deleteJob(UUID cronTaskConfigurationUuid);

    GroovyScriptNamesDto getGroovyScriptsName();
}
