//package com.veadan.folib.cron.jobs;
//
//import com.google.common.collect.ImmutableSet;
//import com.veadan.folib.configuration.ConfigurationManager;
//import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
//import com.veadan.folib.cron.jobs.cleanup.CleanupArtifactsProviderRegistry;
//import com.veadan.folib.cron.jobs.fields.*;
//import com.veadan.folib.domain.Artifact;
//import com.veadan.folib.providers.io.RepositoryFiles;
//import com.veadan.folib.providers.io.RepositoryPath;
//import com.veadan.folib.providers.io.RepositoryPathResolver;
//import com.veadan.folib.providers.layout.DockerFileSystemProvider;
//import com.veadan.folib.util.RepositoryPathUtil;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.collections4.CollectionUtils;
//import org.apache.commons.compress.utils.Lists;
//import org.apache.commons.lang3.StringUtils;
//import org.apache.commons.lang3.exception.ExceptionUtils;
//import org.springframework.beans.factory.annotation.Qualifier;
//
//import javax.inject.Inject;
//import java.nio.file.Files;
//import java.util.List;
//import java.util.Objects;
//import java.util.Set;
//
///**
// * @author leipenghui
// **/
//@Slf4j
//public class CleanupDockerRepositoryCronJob extends JavaCronJob {
//
//    private static final String PROPERTY_STORAGE_ID = "storageId";
//
//    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";
//
//    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
//            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
//                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
//            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
//                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));
//    @Inject
//    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;
//
////    @Inject
////    @Qualifier("dockerFileSystemProvider")
////    private DockerFileSystemProvider dockerFileSystemProvider;
//
//    @Inject
//    private RepositoryPathResolver repositoryPathResolver;
//
//    @Inject
//    private ConfigurationManager configurationManager;
//
//    @Override
//    public void executeTask(CronTaskConfigurationDto config)
//            throws Throwable {
//        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
//        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
//        log.info("Start docker clean artifact job [{} {}]", storageId, repositoryId);
//        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
//            try {
//                RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
//                List<String> paths = RepositoryPathUtil.getFileRelativePaths(rootRepositoryPath);
//                if (CollectionUtils.isEmpty(paths)) {
//                    log.info("Docker仓库 [{}] [{}] 下没有找到制品文件", storageId, repositoryId);
//                    return;
//                }
//                log.info("Start docker clean artifact job [ storageId {} repositoryId {} paths {}]", storageId, repositoryId, paths.size());
//                List<String> resultList = Lists.newArrayList();
//                for (String path : paths) {
//                    try {
//                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
//                        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
//                            log.warn("Cleanup docker storageId {} repositoryId {} path {} file not exists", storageId, repositoryId, path);
//                            continue;
//                        }
//                        if (RepositoryFiles.isTrash(repositoryPath)) {
//                            log.info("Cleanup docker storageId {} repositoryId {} path {} is trash file skip", storageId, repositoryId, path);
//                            continue;
//                        }
//                        if (RepositoryFiles.isTemp(repositoryPath)) {
//                            log.info("Cleanup docker storageId {} repositoryId {} path {} is temp file skip", storageId, repositoryId, path);
//                            continue;
//                        }
//                        if (RepositoryFiles.isChecksum(repositoryPath)) {
//                            log.info("Cleanup docker storageId {} repositoryId {} path {} is checksum file skip", storageId, repositoryId, path);
//                            continue;
//                        }
//                        Artifact artifact = repositoryPath.getArtifactEntry();
//                        if (null == artifact || null == artifact.getLastUpdated()) {
//                            log.warn("Cleanup docker storageId {} repositoryId {} path {} artifact not found", storageId, repositoryId, path);
//                            continue;
//                        }
//                        String artifactPath = artifact.getArtifactPath();
//                        boolean isDockerManifest = artifactPath.contains("sha256:") && artifactPath.contains("manifest/sha256");
//                        if (!isDockerManifest) {
//                            log.info("Cleanup docker storageId {} repositoryId {} path {} not a docker manifest file skip", storageId, repositoryId, path);
//                            continue;
//                        }
//                        log.info("Cleanup docker storageId {} repositoryId {} path {} a docker manifest", storageId, repositoryId, artifactPath);
////                        dockerFileSystemProvider.handlerManifestAndBlob(repositoryPath, true, repositoryPath);
//                    } catch (Exception ex) {
//                        log.error("Clean docker artifact job [{} {} {}] error {}", storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
//                    }
//                }
//                String successMsg = "ok", failMsg = "fail";
//                long success = resultList.stream().filter(successMsg::equals).count(), fail = resultList.stream().filter(failMsg::equals).count(), other = 0;
//                other = resultList.size() - success - fail;
//                log.info("[{}] [{}] docker清理任务 成功 {} 失败 {} 其他 {}",
//                        storageId, repositoryId, success, fail, other);
//            } catch (Exception e) {
//                log.error("Clean docker artifact job [{} {}] error {}", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
//            }
//        } else {
//            log.warn("Docker仓库清理任务不生效，请重新配置!");
//        }
//        log.info("Clean docker end [{} {}]", storageId, repositoryId);
//    }
//
//    @Override
//    public CronJobDefinition getCronJobDefinition() {
//        return CronJobDefinition.newBuilder()
//                .jobClass(CleanupDockerRepositoryCronJob.class.getName())
//                .name("Docker仓库清理任务")
//                .scope(DOCKER)
//                .description("该任务可定时删除Docker制品仓库下的无用制品文件")
//                .fields(FIELDS)
//                .build();
//    }
//}
