package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.providers.repository.proxied.LocalStorageProxyRepositoryExpiredArtifactsCleaner;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;

import javax.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class CleanupExpiredArtifactsFromProxyRepositoriesCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_LAST_ACCESSED_TIME_IN_DAYS = "lastAccessedTimeInDays";

    private static final String PROPERTY_MIN_SIZE_IN_BYTES = "minSizeInBytes";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobIntegerTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_LAST_ACCESSED_TIME_IN_DAYS))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_MIN_SIZE_IN_BYTES))));

    @Inject
    private LocalStorageProxyRepositoryExpiredArtifactsCleaner proxyRepositoryObsoleteArtifactsCleaner;

    @Override
    public void executeTask(final CronTaskConfigurationDto config)
            throws Throwable
    {
        final String lastAccessedTimeInDaysText = config.getRequiredProperty(PROPERTY_LAST_ACCESSED_TIME_IN_DAYS);
        final String minSizeInBytesText = config.getProperty(PROPERTY_MIN_SIZE_IN_BYTES);

        final Integer lastAccessedTimeInDays;
        try
        {
            lastAccessedTimeInDays = Integer.valueOf(lastAccessedTimeInDaysText);
        }
        catch (NumberFormatException ex)
        {
            logger.error("Invalid integer value [{}] of 'lastAccessedTimeInDays' property. Cron job won't be fired.",
                         lastAccessedTimeInDaysText, ex);
            return;
        }

        Long minSizeInBytes = Long.valueOf(-1);
        if (minSizeInBytesText != null)
        {
            try
            {
                minSizeInBytes = Long.valueOf(minSizeInBytesText);
            }
            catch (NumberFormatException ex)
            {
                logger.error("Invalid long value [{}] of 'minSizeInBytes' property. Cron job won't be fired.",
                             minSizeInBytesText, ex);
                return;
            }
        }

        proxyRepositoryObsoleteArtifactsCleaner.cleanup(lastAccessedTimeInDays, minSizeInBytes);
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(CleanupExpiredArtifactsFromProxyRepositoriesCronJob.class.getName())
                                .name("代理仓库清理过期制品")
                                .scope("SYSTEM")
                                .description("可设定过期日期，该任务会清理过期日期之前的制品包")
                                .fields(FIELDS)
                                .build();
    }

}
