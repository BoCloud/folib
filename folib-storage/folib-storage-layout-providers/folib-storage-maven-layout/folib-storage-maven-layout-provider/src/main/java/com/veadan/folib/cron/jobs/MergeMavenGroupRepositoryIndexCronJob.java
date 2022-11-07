package com.veadan.folib.cron.jobs;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.indexing.RepositoryIndexCreator;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Inject;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
public class MergeMavenGroupRepositoryIndexCronJob
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
    @RepositoryIndexCreator.RepositoryIndexCreatorQualifier(RepositoryTypeEnum.GROUP)
    private RepositoryIndexCreator repositoryIndexCreator;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable
    {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        logger.debug("Executing MergeMavenGroupRepositoryIndexCronJob for storageId = [{}], repositoryId = [{}]",
                     storageId, repositoryId);

        Repository repository = configurationManager.getRepository(storageId, repositoryId);

        if (!repository.isGroupRepository())
        {
            logger.warn("Repository identified by storageId = [{}], repositoryId = [{}] is not a group repository. Exiting ...",
                        storageId, repositoryId);
            return;
        }

        repositoryIndexCreator.apply(repository);
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(MergeMavenGroupRepositoryIndexCronJob.class.getName())
                                .name("定时合并Maven组合仓库的索引作业").scope(MAVEN)
                                .description("该任务用于合并Maven组合仓库的索引，只有组合仓库才使用。")
                                .fields(FIELDS)
                                .build();
    }
}
