package com.veadan.folib.cron.jobs;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.cron.services.JobManager;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

/**
 * @author Kate Novik
 */
public class RebuildMavenMetadataCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_BASE_PATH))));

    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private JobManager manager;


    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);

        if (storageId == null)
        {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet())
            {
                rebuildRepositories(storage);
            }
        }
        else if (repositoryId == null)
        {
            rebuildRepositories(storageId);
        }
        else
        {
            artifactMetadataService.rebuildMetadata(storageId, repositoryId, basePath);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RebuildMavenMetadataCronJob.class.getName())
                                .name("定时重建Maven仓库制品包的Metadata任务").scope(MAVEN)
                                .description("该任务用于定时重建Maven仓库制品包的Metadata任务")
                                .fields(FIELDS)
                                .build();
    }

    /**
     * To rebuild artifact's metadata in repositories
     *
     * @param storageId path of storage
     * @throws NoSuchAlgorithmException
     * @throws XmlPullParserException
     * @throws IOException
     */
    private void rebuildRepositories(String storageId)
            throws NoSuchAlgorithmException, XmlPullParserException, IOException
    {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        for (String repository : repositories.keySet())
        {
            artifactMetadataService.rebuildMetadata(storageId, repository, null);
        }
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
