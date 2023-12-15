package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.cron.jobs.fields.CronJobIntegerTypeField;
import com.veadan.folib.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;

import javax.inject.Inject;
import java.util.Set;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 13:23
 * @since x.x.x
 */
public class ClearArtifactSyncRecordCronJob extends JavaCronJob {
    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";
    private static final String RECORD_RETENTION_TIME = "recordRetentionTime";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobIntegerTypeField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(RECORD_RETENTION_TIME))))
    );

    @Inject
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;
    @Inject
    private ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;

    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {

    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(ClearArtifactSyncRecordCronJob.class.getName())
                .name("定时清除晋级/分发记录表数据任务").scope(GLOBAL)
                .description("用于定时定时清除晋级/分发记录表数据，防止记录表数据过大")
                .fields(FIELDS)
                .build();
    }
}
