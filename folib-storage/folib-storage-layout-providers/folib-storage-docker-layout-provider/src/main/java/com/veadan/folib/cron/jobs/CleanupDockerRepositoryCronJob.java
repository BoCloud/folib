package com.veadan.folib.cron.jobs;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.Manifests;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
public class CleanupDockerRepositoryCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            log.info("Docker clean artifact job single repository [{}] [{}]", storageId, repositoryId);
            cleanupDockerRepository(storageId, repositoryId);
        } else {
            log.info("Docker clean artifact job all repository");
            cleanupDockerRepository();
        }
    }

    public void cleanupDockerRepository(String storageId, String repositoryId) {
        log.info("Start docker clean artifact job repository [{}] [{}]", storageId, repositoryId);
        Repository repository = configurationManagementService.getConfiguration().getStorage(storageId).getRepository(repositoryId);
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            if (!Files.exists(rootRepositoryPath)) {
                return;
            }
            handleManifest(rootRepositoryPath);
            handleBlobs(rootRepositoryPath);
        }
    }

    public void cleanupDockerRepository() {
        try {
            for (Map.Entry<String, Storage> entry : configurationManagementService.getConfiguration().getStorages().entrySet()) {
                try {
                    Storage storage = entry.getValue();
                    final Map<String, ? extends Repository> repositories = storage.getRepositories();
                    for (Repository repository : repositories.values()) {
                        try {
                            if (DockerLayoutProvider.ALIAS.equals(repository.getLayout()) && RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                                cleanupDockerRepository(repository.getStorage().getId(), repository.getId());
                            }
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    }
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    private boolean compareTime(RepositoryPath repositoryPath) {
        boolean confirmDelete = false;
        int minusHours = 2;
        LocalDateTime updateDateTime = null;
        try {
            Artifact artifact = repositoryPath.getArtifactEntry();
            if (Objects.nonNull(artifact)) {
                //获取仓库下制品最近使用时间比较
                updateDateTime = artifact.getLastUsed();
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        if (Objects.isNull(updateDateTime)) {
            updateDateTime = getFileUpdateTime(repositoryPath);
        }
        log.info("RepositoryPath [{}] updateDateTime [{}]", repositoryPath.toString(), updateDateTime);
        if (Objects.isNull(updateDateTime)) {
            return true;
        }
        //删除2小时前的制品
        if (!LocalDateTime.now().minusHours(minusHours).isBefore(updateDateTime)) {
            confirmDelete = true;
        }
        return confirmDelete;
    }

    private LocalDateTime getFileUpdateTime(RepositoryPath repositoryPath) {
        LocalDateTime lastModifiedDateTime = null;
        try {
            BasicFileAttributes attributes = Files.readAttributes(repositoryPath, BasicFileAttributes.class);
            FileTime fileTime = attributes.lastModifiedTime();
            // 将FileTime转换为Instant
            Instant instant = fileTime.toInstant();
            // 将Instant转换为LocalDateTime
            lastModifiedDateTime = instant.atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime();
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return lastModifiedDateTime;
    }

    private boolean delete(RepositoryPath repositoryPath, boolean ignoreTime) throws Exception {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Docker repository [{}] [{}] repositoryPath [{}] not exists skip delete", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.toString());
            return false;
        }
        if (ignoreTime) {
            artifactManagementService.delete(repositoryPath, true);
            return true;
        }
        if (compareTime(repositoryPath)) {
            artifactManagementService.delete(repositoryPath, true);
            return true;
        }
        return false;
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(CleanupDockerRepositoryCronJob.class.getName())
                .name("Docker仓库清理任务")
                .scope(DOCKER)
                .description("该任务可定时删除Docker制品仓库下的无用制品文件")
                .fields(FIELDS)
                .build();
    }

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws IOException {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Clean docker artifact job repository [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.toString());
            return null;
        }
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId(), artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        String imageName = dockerArtifactCoordinates.getName();
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = Files.readString(repositoryPath);
        if (!JSONUtil.isJson(manifestString)) {
            log.warn("Clean docker artifact job repository [{}] [{}] manifest [{}] not is json format, The image is damaged and will be deleted", storageId, repositoryId, artifactPath);
            return null;
        }
        ImageManifest imageManifest = null;
        try {
            imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        } catch (Exception ex) {
            log.warn("Clean docker artifact job repository [{}] [{}] manifest [{}] content [{}] parse error [{}]", storageId, repositoryId, artifactPath, manifestString, ExceptionUtils.getStackTrace(ex));
            return null;
        }
        RepositoryPath manifestRootRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.MANIFEST);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = manifestRootRepositoryPath.resolve(manifests.getDigest());
                if (!Files.exists(manifestPath)) {
                    log.warn("Clean docker artifact job repository [{}] [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", storageId, repositoryId, imageName, dockerArtifactCoordinates.getLayers());
                    return null;
                }
                manifestString = Files.readString(manifestPath);
                itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                itemImageManifest.setDigest(manifests.getDigest());
                imageManifestList.add(itemImageManifest);
            }
        }
        if (!Files.exists(manifestRootRepositoryPath.resolve(dockerArtifactCoordinates.getLayers()))) {
            log.warn("Clean docker artifact job repository [{}] [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", storageId, repositoryId, imageName, dockerArtifactCoordinates.getLayers());
            return null;
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }

    private void handleManifest(RepositoryPath rootRepositoryPath) {
        RepositoryPath manifestRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
        if (!Files.exists(manifestRootRepositoryPath)) {
            return;
        }
        //遍历仓库下manifest目录
        try (Stream<Path> manifestPathStream = Files.list(manifestRootRepositoryPath)) {
            AtomicLong manifestSuccessAtomicLong = new AtomicLong(0), manifestFailAtomicLong = new AtomicLong(0);
            manifestPathStream.forEach(manifestPath -> {
                RepositoryPath manifestRepositoryPath = (RepositoryPath) manifestPath;
                try {
                    if (RepositoryFiles.isChecksum(manifestRepositoryPath) || RepositoryFiles.isArtifactMetadata(manifestRepositoryPath) || !DockerArtifactCoordinates.isRealManifestPath(manifestPath)) {
                        return;
                    }
                    log.info("Manifest repositoryPath [{}] [{}] [{}]", manifestRepositoryPath.getStorageId(), manifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(manifestRepositoryPath));
                    AtomicBoolean atomicBoolean = new AtomicBoolean(false);
                    String manifestName = manifestRepositoryPath.getFileName().toString();
                    //遍历仓库下所有的tag，判断是否有tag引用当前manifest
                    Files.walkFileTree(rootRepositoryPath, new SimpleFileVisitor<Path>() {
                        @Override
                        public FileVisitResult visitFile(Path file,
                                                         BasicFileAttributes attrs)
                                throws IOException {
                            RepositoryPath itemPath = (RepositoryPath) file;
                            if (DockerArtifactCoordinates.isTagPath(itemPath)) {
                                log.info("Tag repositoryPath [{}] [{}] [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                                List<ImageManifest> imageManifestList = getImageManifests(itemPath);
                                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                                    log.info("Tag repositoryPath [{}] [{}] [{}] find match manifest [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath), manifestName);
                                    atomicBoolean.set(imageManifestList.stream().filter(item -> StringUtils.isNotBlank(item.getDigest())).anyMatch(item -> manifestName.equals(item.getDigest())));
                                }
                            }
                            if (atomicBoolean.get()) {
                                return FileVisitResult.TERMINATE;
                            }
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                            RepositoryPath itemPath = (RepositoryPath) dir;
                            if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, true) || (DockerArtifactCoordinates.DOCKER_LAYER_DIR_NAME_LIST.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                                log.info("RepositoryPath [{}] skip...", itemPath.toString());
                                return FileVisitResult.SKIP_SUBTREE;
                            }
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult postVisitDirectory(Path dir,
                                                                  IOException exc)
                                throws IOException {
                            return FileVisitResult.CONTINUE;
                        }
                    });
                    if (!atomicBoolean.get()) {
                        try {
                            //manifest不存在引用，删除
                            String artifactPath = RepositoryFiles.relativizePath(manifestRepositoryPath);
                            if (delete(manifestRepositoryPath, false)) {
                                logger.info("Delete manifest repositoryPath [{}] [{}] [{}]", manifestRepositoryPath.getStorageId(), manifestRepositoryPath.getRepositoryId(), artifactPath);
                                manifestSuccessAtomicLong.getAndIncrement();
                            }
                        } catch (IOException e) {
                            manifestFailAtomicLong.getAndIncrement();
                            log.warn(ExceptionUtils.getStackTrace(e));
                        }
                    }
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
            });
            long success = manifestSuccessAtomicLong.get(), fail = manifestFailAtomicLong.get();
            log.info("Clean docker artifact job repository [{}] [{}] manifest finished success [{}] fail [{}]",
                    rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), success, fail);
        } catch (Exception e) {
            log.error("Clean docker artifact job repository [{}] [{}] manifest error [{}]", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), ExceptionUtils.getStackTrace(e));
        }
    }

    private void handleBlobs(RepositoryPath rootRepositoryPath) {
        RepositoryPath blobsRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS);
        if (!Files.exists(blobsRootRepositoryPath)) {
            return;
        }
        RepositoryPath manifestRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
        if (!Files.exists(manifestRootRepositoryPath)) {
            return;
        }
        //遍历仓库下blobs目录
        try (Stream<Path> blobsPathStream = Files.list(blobsRootRepositoryPath)) {
            AtomicLong blobsDeleteSuccessAtomicLong = new AtomicLong(0), blobsDeleteFailAtomicLong = new AtomicLong(0);
            blobsPathStream.forEach(blobPath -> {
                RepositoryPath blobsRepositoryPath = (RepositoryPath) blobPath;
                try {
                    String blobName = blobPath.getFileName().toString();
                    if (RepositoryFiles.isChecksum(blobsRepositoryPath) || RepositoryFiles.isArtifactMetadata(blobsRepositoryPath) || !DockerArtifactCoordinates.include(blobName)) {
                        return;
                    }
                    log.info("Blob repositoryPath [{}] [{}] [{}]", blobsRepositoryPath.getStorageId(), blobsRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(blobsRepositoryPath));
                    AtomicBoolean atomicBoolean = new AtomicBoolean(false);
                    //遍历仓库下的manifest目录，判断是否有manifest引用当前blob
                    Files.walkFileTree(manifestRootRepositoryPath, new SimpleFileVisitor<Path>() {
                        @Override
                        public FileVisitResult visitFile(Path file,
                                                         BasicFileAttributes attrs)
                                throws IOException {
                            RepositoryPath itemPath = (RepositoryPath) file;
                            if (DockerArtifactCoordinates.isRealManifestPath(itemPath)) {
                                log.info("Manifest repositoryPath [{}] [{}] [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                                List<ImageManifest> imageManifestList = getImageManifests(itemPath);
                                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                                    log.info("Manifest repositoryPath [{}] [{}] [{}] find match blob [{}]", itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath), blobName);
                                    final boolean flag = imageManifestList.stream().filter(item -> Objects.nonNull(item.getConfig())).map(item -> item.getConfig().getDigest()).anyMatch(blobName::equals) || imageManifestList.stream().filter(item -> CollectionUtils.isNotEmpty(item.getLayers())).flatMap(item -> item.getLayers().stream()).anyMatch(item -> blobName.equals(item.getDigest()));
                                    atomicBoolean.set(flag);
                                }
                            }
                            if (atomicBoolean.get()) {
                                return FileVisitResult.TERMINATE;
                            }
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                            return FileVisitResult.CONTINUE;
                        }

                        @Override
                        public FileVisitResult postVisitDirectory(Path dir,
                                                                  IOException exc)
                                throws IOException {
                            return FileVisitResult.CONTINUE;
                        }
                    });
                    if (!atomicBoolean.get()) {
                        try {
                            //blob不存在引用，删除
                            String artifactPath = RepositoryFiles.relativizePath(blobsRepositoryPath);
                            if (delete(blobsRepositoryPath, false)) {
                                logger.info("Delete blob repositoryPath [{}] [{}] [{}]", blobsRepositoryPath.getStorageId(), blobsRepositoryPath.getRepositoryId(), artifactPath);
                                blobsDeleteSuccessAtomicLong.getAndIncrement();
                            }
                        } catch (Exception e) {
                            blobsDeleteFailAtomicLong.getAndIncrement();
                            log.warn(ExceptionUtils.getStackTrace(e));
                        }
                    }
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
            });
            long success = blobsDeleteSuccessAtomicLong.get(), fail = blobsDeleteFailAtomicLong.get();
            log.info("Clean docker artifact job repository [{}] [{}] blobs finished success [{}] fail [{}]",
                    rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), success, fail);
        } catch (Exception e) {
            log.error("Clean docker artifact job repository [{}] [{}] blobs error [{}]", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), ExceptionUtils.getStackTrace(e));
        }
    }
}