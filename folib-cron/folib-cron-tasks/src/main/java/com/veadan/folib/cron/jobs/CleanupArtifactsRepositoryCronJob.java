package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.util.RepositoryPathUtil;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

/**
 * 清理保存N 天前的制品
 *
 * @author qijianping
 * @date 2023/02/01
 */
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
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ThreadPoolTaskExecutor asyncRepositoryCleanupThreadPoolExecutor;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String cleanDay = config.getProperty(PROPERTY_STORAGE_DAY);
        logger.info("start clean artifact job [{}  {}  {}]", storageId, repositoryId, cleanDay);
        if (storageId != null && repositoryId != null) {
            logger.info("start CleanupArtifactsRepositoryCronJob ");
            cleanRepositoryByDay(storageId, repositoryId, cleanDay);
        } else {
            logger.info("仓库自定义清理任务不生效，请重新配置!");
        }
        logger.info("Clean end [{}  {}  {}]", storageId, repositoryId, cleanDay);
    }

    private void cleanRepositoryByDay(String storageId, String repositoryId, String cleanDay) {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
            List<String> paths = RepositoryPathUtil.getFileRelativePaths(repositoryPath);
            List<FutureTask<String>> listTask = new ArrayList<>();
            if (null == paths || paths.size() == 0) {
                logger.warn("仓库下没有找到制品文件");
                return;
            }
            for (String x : paths) {
                RepositoryCleanUpTask repositoryCleanUpTask =
                        new RepositoryCleanUpTask(storageId, repositoryId, cleanDay, x,
                                repositoryPathResolver, artifactManagementService);
                FutureTask<String> futureTask = new FutureTask<String>(repositoryCleanUpTask);
                listTask.add(futureTask);
                asyncRepositoryCleanupThreadPoolExecutor.submit(futureTask);
            }
            int success = 0;
            int fail = 0;
            for (FutureTask<String> task : listTask) {
                try {
                    String rs = task.get();
                    if (rs.equals("ok")) {
                        success++;
                    } else if (rs.equals("fail")) {
                        fail++;
                    }
                } catch (Exception e) {
                    fail++;
                    logger.error("清理失败 {}", e.getMessage());
                }
            }
            logger.info("[{}] [{}] [{}] 自定义清理任务成功 {} 失败 {}",
                    storageId, repositoryId, cleanDay, success, fail);
            listTask.clear();
        } catch (Exception e) {
            e.printStackTrace();
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

    class RepositoryCleanUpTask implements Callable<String> {
        private String storageId;
        private String repositoryId;
        private String cleanDay;
        private String path;
        private RepositoryPathResolver repositoryPathResolver;
        private ArtifactManagementService artifactManagementService;

        public RepositoryCleanUpTask(String storageId, String repositoryId, String cleanDay, String path,
                                     RepositoryPathResolver repositoryPathResolver,
                                     ArtifactManagementService artifactManagementService) {
            this.storageId = storageId;
            this.repositoryId = repositoryId;
            this.cleanDay = cleanDay;
            this.path = path;
            this.repositoryPathResolver = repositoryPathResolver;
            this.artifactManagementService = artifactManagementService;
        }

        @Override
        public String call() throws Exception {
            logger.info("cleanup repo task start [{}/{}/{}]", storageId, repositoryId, path);
            long tempDay = Long.parseLong(cleanDay);
            RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            Artifact artifact = artifactPath.getArtifactEntry();
            if (null == artifact || null == artifact.getLastUpdated()) {
                return "";
            }
            // 获取仓库下制品更新时间 作比较
            LocalDateTime localDateTime = artifactPath.getArtifactEntry().getLastUpdated();

            // 保留 N 天的制品
            if (!LocalDateTime.now().minusDays(tempDay).isBefore(localDateTime)) {
                try {
                    artifactManagementService.delete(artifactPath, true);
                } catch (Exception e) {
                    logger.error("自定义仓库清理路径失败 {}", e.getMessage());
                    return "fail";
                }
            }
            return "ok";
        }
    }

}