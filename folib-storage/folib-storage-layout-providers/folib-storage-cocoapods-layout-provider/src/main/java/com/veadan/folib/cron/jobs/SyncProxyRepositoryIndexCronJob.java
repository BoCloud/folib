package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.CocoapodsIndexService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.util.Set;

/**
 * 通过代理远程仓库索引定时任务
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/9/21 13:30
 * @since x.x.x
 */
public class SyncProxyRepositoryIndexCronJob extends JavaCronJob
{
    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Inject
    private CocoapodsIndexService cocoapodsIndexService;
    
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ConfigurationManager configurationManager;

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));
    
    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable 
    {
        logger.info("定时任务，执行定时任务（{}）开始", this.getCronJobDefinition().getName());
        final String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        final String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        final Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (cocoapodsIndexService.getSyncProxyIndexLock(repository))
        {
            logger.info("定时任务，已存在正在执行的同步远程仓库（{}）任务， 跳过当次定时任务执行", String.format("%s:%s", storageId, repositoryId));
            return;
        }
        final boolean syncProxyIndexResult = cocoapodsIndexService.syncProxyIndex(repository);
        logger.info("定时任务，同步远程仓库（{}）{}", String.format("%s:%s", storageId, repositoryId),syncProxyIndexResult?"成功":"失败");
        logger.info("定时任务，执行定时任务（{}）结束", this.getCronJobDefinition().getName());
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(SyncProxyRepositoryIndexCronJob.class.getName())
                .name("定时同步Cocoapods远程仓库索引的任务").scope(COCOAPODS)
                .description("用于定时同步Cocoapods远程仓库索引到Folib仓库")
                .fields(FIELDS)
                .build();
    }
}
