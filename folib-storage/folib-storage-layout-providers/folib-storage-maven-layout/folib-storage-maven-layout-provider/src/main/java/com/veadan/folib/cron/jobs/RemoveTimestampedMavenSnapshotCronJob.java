package com.veadan.folib.cron.jobs;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryPolicyEnum;

import javax.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

/**
 * @author Kate Novik.
 */
public class RemoveTimestampedMavenSnapshotCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_NUMBER_TO_KEEP = "numberToKeep";

    private static final String PROPERTY_KEEP_PERIOD = "keepPeriod";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_BASE_PATH))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_NUMBER_TO_KEEP))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_KEEP_PERIOD))));

    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);

        // The number of artifacts to keep
        int numberToKeep = config.getProperty(PROPERTY_NUMBER_TO_KEEP) != null ?
                           Integer.valueOf(config.getProperty(PROPERTY_NUMBER_TO_KEEP)) :
                           10;

        // The period to keep artifacts (the number of days)
        int keepPeriod = config.getProperty(PROPERTY_KEEP_PERIOD) != null ?
                         Integer.valueOf(config.getProperty(PROPERTY_KEEP_PERIOD)) :
                         30;

        if (storageId == null)
        {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet())
            {
                removeTimestampedSnapshotArtifacts(storage, numberToKeep, keepPeriod);
            }
        }
        else if (repositoryId == null)
        {
            removeTimestampedSnapshotArtifacts(storageId, numberToKeep, keepPeriod);
        }
        else
        {
            mavenRepositoryFeatures.removeTimestampedSnapshots(storageId,
                                                               repositoryId,
                                                               basePath,
                                                               numberToKeep,
                                                               keepPeriod);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RemoveTimestampedMavenSnapshotCronJob.class.getName())
                                .name("定时删除SNAPSHOT的Maven制品").scope(MAVEN)
                                .description("定时删除SNAPSHOT，带有时间日期快照的制品包")
                                .fields(FIELDS)
                                .build();
    }

    /**
     * To remove timestamped snapshot artifacts in repositories
     *
     * @param storageId    path of storage
     * @param numberToKeep the number of artifacts to keep
     * @param keepPeriod   the period to keep artifacts (the number of days)
     * @throws NoSuchAlgorithmException
     * @throws XmlPullParserException
     * @throws IOException
     */
    private void removeTimestampedSnapshotArtifacts(String storageId,
                                                    int numberToKeep,
                                                    int keepPeriod)
            throws NoSuchAlgorithmException,
                   XmlPullParserException,
                   IOException
    {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        repositories.forEach((repositoryId, repository) ->
                             {
                                 if (repository.getPolicy().equals(RepositoryPolicyEnum.SNAPSHOT.getPolicy()))
                                 {
                                     try
                                     {
                                         mavenRepositoryFeatures.removeTimestampedSnapshots(storageId,
                                                                                            repositoryId,
                                                                                            null,
                                                                                            numberToKeep,
                                                                                            keepPeriod);
                                     }
                                     catch (IOException e)
                                     {
                                         logger.error(e.getMessage(), e);
                                     }
                                 }
                             });
    }

    private Map<String, Storage> getStorages()
    {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId)
    {
        return getStorages().get(storageId).getRepositories();
    }

}
