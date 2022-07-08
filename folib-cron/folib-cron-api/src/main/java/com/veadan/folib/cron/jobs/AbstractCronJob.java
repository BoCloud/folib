package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.cron.services.JobManager;
import com.veadan.folib.event.cron.CronTaskEventListenerRegistry;
import com.veadan.folib.cron.CronJobStatusEnum;

import javax.inject.Inject;
import java.util.Set;
import java.util.UUID;

import com.google.common.collect.ImmutableSet;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.InterruptableJob;
import org.quartz.JobExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.env.Profiles;
import org.springframework.scheduling.quartz.QuartzJobBean;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author Veadan
 */
@DisallowConcurrentExecution
@Transactional("cronJobTransactionManager")
public abstract class AbstractCronJob
        extends QuartzJobBean
        implements InterruptableJob
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    private CronTaskEventListenerRegistry cronTaskEventListenerRegistry;

    @Inject
    private JobManager manager;

    @Inject
    private Environment environment;

    @Inject
    protected CronTaskConfigurationService cronTaskConfigurationService;

    private String status = CronJobStatusEnum.SLEEPING.getStatus();

    protected abstract void executeTask(CronTaskConfigurationDto config)
            throws Throwable;

    @Override
    protected void executeInternal(JobExecutionContext jobExecutionContext)
    {

        final String jobKey = jobExecutionContext.getJobDetail().getKey().getName();
        final UUID jobKeyUuid = UUID.fromString(jobKey);

        CronTaskConfigurationDto configuration = cronTaskConfigurationService.getTaskConfigurationDto(jobKeyUuid);

        if (configuration == null)
        {
            configuration = (CronTaskConfigurationDto) jobExecutionContext.getJobDetail().getJobDataMap().get("config");
        }
        if (configuration == null)
        {
            logger.info("Configuration not found for UUID [{}].", jobKeyUuid);

            return;
        }

        if (!enabled(configuration, environment))
        {
            logger.info("Cron job [{}] disabled, skip execution.", configuration.getName());

            return;
        }

        logger.info("Cron job [{}] enabled, executing.", configuration.getName());

        setStatus(CronJobStatusEnum.EXECUTING.getStatus());
        cronTaskEventListenerRegistry.dispatchCronTaskExecutingEvent(configuration.getUuid());

        try
        {
            executeTask(configuration);
            logger.info("Cron job task [{}] execution completed.", configuration.getName());
        }
        catch (Throwable e)
        {
            logger.error("Failed to execute cron job task [{}].", configuration.getName(), e);
        }
        manager.addExecutedJob(configuration.getUuid().toString(), true);

        cronTaskEventListenerRegistry.dispatchCronTaskExecutedEvent(configuration.getUuid());
        setStatus(CronJobStatusEnum.SLEEPING.getStatus());

    }

    @Override
    public void interrupt()
    {
    }

    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env)
    {
        return configuration.isOneTimeExecution() || !env.acceptsProfiles(Profiles.of("test"));
    }

    public String getStatus()
    {
        return status;
    }

    public void setStatus(String status)
    {
        this.status = status;
    }

    public abstract CronJobDefinition getCronJobDefinition();

    public Set<CronJobDuplicationCheckStrategy> getDuplicationStrategies()
    {
        return ImmutableSet.of(PerRepositoryDuplicationCheckStrategy.getDefault());
    }

}
