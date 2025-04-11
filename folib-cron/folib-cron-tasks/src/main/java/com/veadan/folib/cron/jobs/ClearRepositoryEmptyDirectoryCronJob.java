package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Maps;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.services.RepositoryManagementService;
import org.apache.commons.lang3.StringUtils;

import javax.inject.Inject;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 */
public class ClearRepositoryEmptyDirectoryCronJob
        extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        if (storageId == null && repositoryId == null) {
            repositoryManagementService.deleteEmptyDirectory();
        } else {
            repositoryManagementService.deleteEmptyDirectory(storageId, repositoryId);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ClearRepositoryEmptyDirectoryCronJob.class.getName())
                .name("仓库空目录定时清理任务")
                .scope(GLOBAL)
                .description("该任务可定时删除制品仓库下遗留的空目录")
                .fields(FIELDS)
                .build();
    }

}
