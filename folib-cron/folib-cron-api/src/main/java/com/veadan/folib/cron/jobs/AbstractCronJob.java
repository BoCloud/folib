package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.cron.CronJobStatusEnum;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.cron.services.JobManager;
import com.veadan.folib.event.cron.CronTaskEventListenerRegistry;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.InterruptableJob;
import org.quartz.JobExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.env.Profiles;
import org.springframework.scheduling.quartz.QuartzJobBean;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * @author Veadan
 */
@DisallowConcurrentExecution
@Transactional("cronJobTransactionManager")
public abstract class AbstractCronJob
        extends QuartzJobBean
        implements InterruptableJob {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    private CronTaskEventListenerRegistry cronTaskEventListenerRegistry;

    @Inject
    private JobManager manager;

    @Inject
    private Environment environment;

    @Inject
    protected CronTaskConfigurationService cronTaskConfigurationService;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    private String status = CronJobStatusEnum.SLEEPING.getStatus();

    protected abstract void executeTask(CronTaskConfigurationDto config)
            throws Throwable;

    @Override
    protected void executeInternal(JobExecutionContext jobExecutionContext) {

        final String jobKey = jobExecutionContext.getJobDetail().getKey().getName();
        final UUID jobKeyUuid = UUID.fromString(jobKey);

        CronTaskConfigurationDto configuration = cronTaskConfigurationService.getTaskConfigurationDto(jobKeyUuid);

        if (configuration == null) {
            configuration = (CronTaskConfigurationDto) jobExecutionContext.getJobDetail().getJobDataMap().get("config");
        }
        if (configuration == null) {
            logger.info("Configuration not found for UUID [{}].", jobKeyUuid);

            return;
        }

        if (!enabled(configuration, environment)) {
            logger.info("Cron job [{}] uuid [{}] disabled, skip execution.", configuration.getName(), jobKeyUuid);

            return;
        }


        String lockName = jobKeyUuid.toString();
        long waitTime = 1L, releaseTime = 8L;
        logger.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime, TimeUnit.SECONDS, releaseTime, TimeUnit.HOURS)) {
            try {
                logger.info("Locked for [{}]", lockName);
                long startTime = System.currentTimeMillis();
                logger.info("Cron job [{}] uuid [{}] enabled, executing.", configuration.getName(), jobKeyUuid);
                setStatus(CronJobStatusEnum.EXECUTING.getStatus());
                cronTaskEventListenerRegistry.dispatchCronTaskExecutingEvent(configuration.getUuid());

                try {
                    executeTask(configuration);
                    logger.info("Cron job task [{}] uuid [{}] execution completed take time [{}] ms.", configuration.getName(), jobKeyUuid, System.currentTimeMillis() - startTime);
                } catch (Throwable e) {
                    logger.error("Failed to execute cron job task [{}] uuid [{}].", configuration.getName(), jobKeyUuid, e);
                }
                manager.addExecutedJob(configuration.getUuid().toString(), true);

                cronTaskEventListenerRegistry.dispatchCronTaskExecutedEvent(configuration.getUuid());
                setStatus(CronJobStatusEnum.SLEEPING.getStatus());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            logger.info("LockName [{}] was not get lock", lockName);
        }

    }

    @Override
    public void interrupt() {
    }

    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env) {
        return configuration.isOneTimeExecution() || !env.acceptsProfiles(Profiles.of("test"));
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public abstract CronJobDefinition getCronJobDefinition();

    public Set<CronJobDuplicationCheckStrategy> getDuplicationStrategies() {
        return ImmutableSet.of(PerRepositoryDuplicationCheckStrategy.getDefault());
    }

}
