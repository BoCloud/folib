package com.folib.job.tasks;

import com.folib.job.cron.jobs.fields.*;
import com.google.common.collect.ImmutableSet;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

import javax.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 */
public class RemoveMavenArtifactCronJob
        extends JavaCronJob {

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
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_BASE_PATH), "基础路径"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_NUMBER_TO_KEEP), "保留版本"))));

    @Inject
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);

        // The number of artifacts to keep
        int numberToKeep = config.getProperty(PROPERTY_NUMBER_TO_KEEP) != null ?
                Integer.parseInt(config.getProperty(PROPERTY_NUMBER_TO_KEEP)) :
                0;

        // The period to keep artifacts (the number of days)
        int keepPeriod = config.getProperty(PROPERTY_KEEP_PERIOD) != null ?
                Integer.parseInt(config.getProperty(PROPERTY_KEEP_PERIOD)) :
                0;

        if (storageId == null) {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet()) {
                removeMavenArtifacts(storage, numberToKeep, keepPeriod);
            }
        } else if (repositoryId == null) {
            removeMavenArtifacts(storageId, numberToKeep, keepPeriod);
        } else {
            mavenRepositoryFeatures.removeMavenArtifact(storageId,
                    repositoryId,
                    basePath,
                    numberToKeep,
                    keepPeriod);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveMavenArtifactCronJob.class.getName())
                .name("定时删除Maven制品任务").scope(MAVEN)
                .description("该任务用于按照版本数量清理Maven制品包")
                .fields(FIELDS)
                .build();
    }

    /**
     * To remove maven artifacts in repositories
     *
     * @param storageId    path of storage
     * @param numberToKeep the number of artifacts to keep
     * @param keepPeriod   the period to keep artifacts (the number of days)
     * @throws NoSuchAlgorithmException
     * @throws XmlPullParserException
     * @throws IOException
     */
    private void removeMavenArtifacts(String storageId,
                                      int numberToKeep,
                                      int keepPeriod)
            throws NoSuchAlgorithmException,
            XmlPullParserException,
            IOException {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        repositories.forEach((repositoryId, repository) ->
        {
            if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                try {
                    mavenRepositoryFeatures.removeMavenArtifact(storageId,
                            repositoryId,
                            null,
                            numberToKeep,
                            keepPeriod);
                } catch (IOException e) {
                    logger.error(e.getMessage(), e);
                }
            }
        });
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }

    private Map<String, ? extends Repository> getRepositories(String storageId) {
        return getStorages().get(storageId).getRepositories();
    }

}
