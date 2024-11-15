package com.veadan.folib.cron.services;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.GroovyScriptNamesDto;

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
