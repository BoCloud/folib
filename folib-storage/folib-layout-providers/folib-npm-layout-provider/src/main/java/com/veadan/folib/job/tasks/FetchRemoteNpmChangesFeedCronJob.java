package com.veadan.folib.job.tasks;

import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.jobs.CronJobDefinition;
import com.veadan.folib.job.cron.jobs.JavaCronJob;
import com.veadan.folib.job.cron.jobs.fields.*;
import com.veadan.folib.repository.NpmRepositoryFeatures;

import javax.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.springframework.core.env.Environment;

/**
 * @author @author veadan
 */
public class FetchRemoteNpmChangesFeedCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private NpmRepositoryFeatures features;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        features.fetchRemoteChangesFeed(storageId, repositoryId);
    }

    public static String calculateJobName(String storageId,
                                          String repositoryId)
    {
        return String.format("Fetch Remote Changes feed for %s:%s", storageId, repositoryId);
    }

    @Override
    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env)
    {
        if (!super.enabled(configuration, env))
        {
            return false;
        }

        return shouldDownloadRemoteChangesFeed();
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(FetchRemoteNpmChangesFeedCronJob.class.getName())
                                .name("定时拉取远程Npm的Feed更改记录").scope(NPM)
                                .description("该任务用于定时拉取远程Npm的Feed更改记录")
                                .fields(FIELDS)
                                .build();
    }

    public static boolean shouldDownloadRemoteChangesFeed()
    {
        return System.getProperty("folib.npm.remote.changes.enabled") == null ||
               Boolean.parseBoolean(System.getProperty("folib.npm.remote.changes.enabled"));
    }
}
