package com.veadan.folib.cron.jobs;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.indexing.RepositoryIndexCreator;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import org.springframework.core.env.Environment;

/**
 * @author Kate Novik
 * @author Veadan
 */
public class DownloadRemoteMavenIndexCronJob
        extends JavaCronJob
{

    public static final String FOLIB_DOWNLOAD_INDEXES = "folib.download.indexes";

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    @RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.PROXY)
    private RepositoryIndexCreator repositoryIndexCreator;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws IOException
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        logger.info("Executing DownloadRemoteMavenIndexCronJob for storageId = [{}], repositoryId = [{}]",
                     storageId, repositoryId);

        Repository repository = configurationManager.getRepository(storageId, repositoryId);

        if (!repository.isProxyRepository())
        {
            logger.warn("Repository identified by storageId = [{}], repositoryId = [{}] is not a proxy repository. Exiting ...",
                        storageId, repositoryId);
            return;
        }

        repositoryIndexCreator.apply(repository);
    }

    @Override
    public boolean enabled(CronTaskConfigurationDto configuration,
                           Environment env)
    {
        if (!super.enabled(configuration, env))
        {
            return false;
        }

        String storageId = configuration.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = configuration.getProperty(PROPERTY_REPOSITORY_ID);

        boolean shouldDownloadIndexes = shouldDownloadAllRemoteRepositoryIndexes();
        boolean shouldDownloadRepositoryIndex = shouldDownloadRepositoryIndex(storageId, repositoryId);

        return shouldDownloadIndexes || shouldDownloadRepositoryIndex;
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(DownloadRemoteMavenIndexCronJob.class.getName())
                                .name("定时拉取远程Maven仓库索引").scope(MAVEN)
                                .description("该任务用于定时拉取远程仓库下的索引文件")
                                .fields(FIELDS)
                                .build();
    }

    public static boolean shouldDownloadAllRemoteRepositoryIndexes()
    {
        return System.getProperty(FOLIB_DOWNLOAD_INDEXES) == null ||
               Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES));
    }

    public static boolean shouldDownloadRepositoryIndex(String storageId,
                                                        String repositoryId)
    {
        return (System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "." + repositoryId) == null ||
                Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "."
                                                        + repositoryId)))
               &&
               isIncludedDespiteWildcard(storageId, repositoryId);
    }

    public static boolean isIncludedDespiteWildcard(String storageId,
                                                    String repositoryId)
    {
        return // is excluded by wildcard
                !Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + ".*")) &&
                // and is explicitly included
                Boolean.parseBoolean(System.getProperty(FOLIB_DOWNLOAD_INDEXES + "." + storageId + "."
                                                        + repositoryId));
    }

}
