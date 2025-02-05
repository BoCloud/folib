package com.veadan.folib.cron.jobs;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.services.ChecksumService;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections4.CollectionUtils;

/**
 * @author veadan
 */
public class RegenerateChecksumCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_FORCE_REGENERATION = "forceRegeneration";

    private static final String PROPERTY_LAST_MODIFIED_TIME = "lastModifiedTime";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobBooleanTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_FORCE_REGENERATION), "是否强制覆盖"))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_BASE_PATH), "仓库相对路径"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_LAST_MODIFIED_TIME), "几天内的制品"))));

    @Inject
    private ChecksumService checksumService;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);
        String lastModifiedTime = config.getProperty(PROPERTY_LAST_MODIFIED_TIME);
        /**
         * The values of forceRegeneration are:
         * - true  - to re-write existing checksum and to regenerate missing checksum,
         * - false - to regenerate missing checksum only
         */
        boolean forceRegeneration = Boolean.valueOf(config.getProperty(PROPERTY_FORCE_REGENERATION));

        if (storageId == null && repositoryId == null) {
            Map<String, Storage> storages = getStorages();
            for (String storage : storages.keySet()) {
                regenerateRepositoriesChecksum(storage, lastModifiedTime, forceRegeneration);
            }
        } else if (storageId != null && repositoryId != null) {
            checksumService.regenerateChecksum(storageId, repositoryId, basePath, lastModifiedTime, forceRegeneration);
        } else {
            throw new IllegalArgumentException("storageId and repositoryId can not be null");
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RegenerateChecksumCronJob.class.getName())
                                .name("定时重新生成制品的Checksum文件")
                                .scope(GLOBAL)
                                .description("该任务用于重新生成制品的Checksum文件")
                                .fields(FIELDS)
                                .build();
    }

    /**
     * To regenerate artifact's checksum in repositories
     *
     * @param storageId         path of storage
     * @param lastModifiedTime  处理多少天内的制品
     * @param forceRegeneration true - to re-write existing checksum and to regenerate missing checksum,
     *                          false - to regenerate missing checksum only
     * @throws IOException
     */
    private void regenerateRepositoriesChecksum(String storageId, String lastModifiedTime,
                                                boolean forceRegeneration)
            throws IOException
    {
        Map<String, ? extends Repository> repositories = getRepositories(storageId);

        for (String repositoryId : repositories.keySet())
        {
            if(!existsRepositoryTask(storageId, repositoryId)){
                checksumService.regenerateChecksum(storageId, repositoryId, null, lastModifiedTime, forceRegeneration);
            }
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

    private boolean existsRepositoryTask(String storageId, String repositoryId) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        String cronJob = "com.veadan.folib.cron.jobs.RegenerateChecksumCronJob";
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && cronJob.equals(cron.getJobClass()));
    }

}
