package com.veadan.folib.job.tasks;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.jobs.CronJobDefinition;
import com.veadan.folib.job.cron.jobs.JavaCronJob;
import com.veadan.folib.job.cron.jobs.fields.*;
import com.veadan.folib.storage.indexing.RepositoryIndexCreator;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import jakarta.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author Kate Novik.
 */
public class RebuildMavenIndexesCronJob
        extends JavaCronJob
{

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobRequiredField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.HOSTED)
    private RepositoryIndexCreator repositoryIndexCreator;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        logger.info("Executing RebuildMavenIndexesCronJob for storageId = [{}], repositoryId = [{}]",
                     storageId, repositoryId);

        Repository repository = configurationManager.getRepository(storageId, repositoryId);

        if (!repository.isHostedRepository())
        {
            logger.warn("Repository identified by storageId = [{}], repositoryId = [{}] is not a hosted repository. Exiting ...",
                        storageId, repositoryId);
            return;
        }

        repositoryIndexCreator.apply(repository);
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(RebuildMavenIndexesCronJob.class.getName())
                                .name("定时重建Maven仓库的索引任务").scope(MAVEN)
                                .description("该任务用于定时重建Maven仓库的索引")
                                .fields(FIELDS)
                                .build();
    }

}
