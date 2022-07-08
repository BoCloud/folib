package com.veadan.folib.cron.services;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.GroovyScriptNamesDto;

import java.util.UUID;

/**
 * @author Veadan
 * @author Pablo Tirado
 */
public interface CronJobSchedulerService
{

    void scheduleJob(CronTaskConfigurationDto cronTaskConfiguration);

    void deleteJob(UUID cronTaskConfigurationUuid);

    GroovyScriptNamesDto getGroovyScriptsName();
}
