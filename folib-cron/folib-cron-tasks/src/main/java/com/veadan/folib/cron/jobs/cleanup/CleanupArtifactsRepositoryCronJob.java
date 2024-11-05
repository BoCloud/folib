package com.veadan.folib.cron.jobs.cleanup;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.CronJobDefinition;
import com.veadan.folib.cron.jobs.JavaCronJob;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;

import javax.inject.Inject;
import java.util.Objects;
import java.util.Set;

/**
 * 清理保存N 天前的制品
 *
 * @author qijianping
 * @date 2023/02/01
 */
@Slf4j
public class CleanupArtifactsRepositoryCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_STORAGE_DAY = "storageDay";

    private static final String PROPERTY_STORAGE_CONDITION = "storageCondition";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_STORAGE_CONDITION), "保留条件"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_STORAGE_DAY), "保留天数"))));

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String storageDay = config.getProperty(PROPERTY_STORAGE_DAY);
        String storageCondition = config.getProperty(PROPERTY_STORAGE_CONDITION);
        log.info("Start clean artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]", storageId, repositoryId, storageCondition, storageDay);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && StringUtils.isNotBlank(storageDay)) {
            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            if (Objects.isNull(repository)) {
                log.warn("Repository storageId [{}] repositoryId [{}] not found", storageId, repositoryId);
                return;
            }
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                log.warn("Repository storageId [{}] repositoryId [{}] is group type skip..", storageId, repositoryId);
                return;
            }
            if (!NumberUtils.isDigits(storageDay) || Integer.parseInt(storageDay) <= 0) {
                log.warn("Params storageDay [{}] error", storageDay);
                return;
            }
            String dockerLayout = "Docker", cleanupRepositoryType = "GENERAL";
            if (dockerLayout.equalsIgnoreCase(repository.getLayout())) {
                cleanupRepositoryType = "DOCKER";
            }
            CleanupArtifactsProvider cleanupArtifactsProvider = cleanupArtifactsProviderRegistry.getProvider(cleanupRepositoryType);
            cleanupArtifactsProvider.cleanup(storageId, repositoryId, "", storageDay, storageCondition);
        } else {
            log.warn("Repository storageId repositoryId storageDay should not be null");
        }
        log.info("Clean artifact job end storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]", storageId, repositoryId, storageCondition, storageDay);
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(CleanupArtifactsRepositoryCronJob.class.getName())
                .name("仓库自定义清理任务")
                .scope(GLOBAL)
                .description("该任务可定时删除制品仓库下的制品文件")
                .fields(FIELDS)
                .build();
    }
}