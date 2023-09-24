package com.veadan.folib.cron.jobs.cleanup;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.CronJobDefinition;
import com.veadan.folib.cron.jobs.JavaCronJob;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import javax.inject.Inject;
import java.util.List;
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

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStorageIdAutocompleteField(new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_DAY)))));
    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ThreadPoolTaskExecutor asyncRepositoryCleanupThreadPoolExecutor;

    @Inject
    private ConfigurationManager configurationManager;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String cleanDay = config.getProperty(PROPERTY_STORAGE_DAY);
        log.info("Start clean artifact job [{} {} {}]", storageId, repositoryId, cleanDay);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            cleanRepositoryByDay(storageId, repositoryId, cleanDay);
        } else {
            log.warn("仓库自定义清理任务不生效，请重新配置!");
        }
        log.info("Clean end [{} {} {}]", storageId, repositoryId, cleanDay);
    }

    private void cleanRepositoryByDay(String storageId, String repositoryId, String cleanDay) {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
            List<String> paths = RepositoryPathUtil.getFileRelativePaths(repositoryPath);
            if (CollectionUtils.isEmpty(paths)) {
                log.info("仓库 [{}] [{}] 下没有找到制品文件", storageId, repositoryId);
                return;
            }
            log.info("Start clean artifact job [ storageId {} repositoryId {} cleanDay {} paths {}]", storageId, repositoryId, cleanDay, paths.size());
            List<String> resultList = Lists.newArrayList();
            for (String path : paths) {
                try {
                    Repository repository = configurationManager.getRepository(storageId, repositoryId);
                    String dockerLayout = "Docker", cleanupRepositoryType = "GENERAL";
                    if (dockerLayout.equalsIgnoreCase(repository.getLayout())) {
                        cleanupRepositoryType = "DOCKER";
                    }
                    CleanupArtifactsProvider cleanupArtifactsProvider = cleanupArtifactsProviderRegistry.getProvider(cleanupRepositoryType);
                    String result = cleanupArtifactsProvider.cleanup(storageId, repositoryId, path, cleanDay);
                    resultList.add(result);
                } catch (Exception ex) {
                    log.error("Clean artifact job [{} {} {} {}] error {}", storageId, repositoryId, cleanDay, path, ExceptionUtils.getStackTrace(ex));
                }
            }
            String successMsg = "ok", failMsg = "fail";
            long success = resultList.stream().filter(successMsg::equals).count(), fail = resultList.stream().filter(failMsg::equals).count(), other = 0;
            other = resultList.size() - success - fail;
            log.info("[{}] [{}] [{}] 自定义清理任务 成功 {} 失败 {} 其他 {}",
                    storageId, repositoryId, cleanDay, success, fail, other);
        } catch (Exception e) {
            log.error("Clean artifact job [{} {} {} ] error {}", storageId, repositoryId, cleanDay, ExceptionUtils.getStackTrace(e));
        }
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