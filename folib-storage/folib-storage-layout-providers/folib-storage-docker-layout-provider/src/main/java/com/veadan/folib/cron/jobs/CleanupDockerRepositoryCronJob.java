package com.veadan.folib.cron.jobs;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerFileSystemProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.schema2.Manifests;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
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

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        log.info("Start docker clean artifact job repository [{}] [{}]", storageId, repositoryId);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            try {
                RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "");
                List<Path> directoryList = getDirectory(rootRepositoryPath, Collections.emptyList());
                if (CollectionUtils.isEmpty(directoryList)) {
                    log.info("Docker repository [{}] [{}] is empty clean end", storageId, repositoryId);
                    return;
                }
                LayoutFileSystemProvider provider = rootRepositoryPath.getFileSystem()
                        .provider();
                DockerFileSystemProvider dockerFileSystemProvider = (DockerFileSystemProvider) provider;
                log.info("Docker repository [{}] [{}] find images quantity [{}]", storageId, repositoryId, directoryList.size());
                List<String> resultList = Lists.newArrayList();
                RepositoryPath imageRepositoryPath, imageTagRepositoryPath, imageManifestsRepositoryPath, imageBlobsRepositoryPath;
                String blobs = "blobs", manifest = "manifest", successMsg = "ok", failMsg = "fail";
                List<String> excludeList = Lists.newArrayList(blobs, manifest);
                List<Path> imageTagList = null;
                List<DockerImageTag> dockerImageTagList = null;
                DockerImageTag dockerImageTag = null;
                for (Path path : directoryList) {
                    try {
                        imageRepositoryPath = (RepositoryPath) path;
                        imageTagList = getDirectory(imageRepositoryPath, excludeList);
                        if (CollectionUtils.isEmpty(imageTagList)) {
                            artifactManagementService.delete(imageRepositoryPath, true);
                            resultList.add(successMsg);
                            log.info("Docker repository [{}] [{}] image [{}] has no tag tag delete", storageId, repositoryId, path.toString());
                            continue;
                        }
                        imageManifestsRepositoryPath = imageRepositoryPath.resolve(manifest);
                        imageBlobsRepositoryPath = imageRepositoryPath.resolve(blobs);
                        dockerImageTagList = Lists.newArrayList();
                        for (Path imageTagPath : imageTagList) {
                            imageTagRepositoryPath = (RepositoryPath) imageTagPath;
                            dockerImageTag = handlerDockerImageTag(imageTagRepositoryPath, imageManifestsRepositoryPath, imageBlobsRepositoryPath, dockerImageTagList);
                            if (Objects.isNull(dockerImageTag)) {
                                artifactManagementService.delete(imageTagRepositoryPath, true);
                                resultList.add(successMsg);
                                log.info("Docker repository [{}] [{}] image [{}] tag [{}] not exists blobs delete", storageId, repositoryId, path.toString(), imageTagPath.getFileName().toString());
                            }
                        }
                        if (CollectionUtils.isEmpty(dockerImageTagList)) {
                            artifactManagementService.delete(imageRepositoryPath, true);
                            resultList.add(successMsg);
                            log.info("Docker repository [{}] [{}] image [{}] not exists blobs delete", storageId, repositoryId, path.toString());
                            continue;
                        }
                        Set<String> manifestSet = Sets.newLinkedHashSet(), blobSet = Sets.newLinkedHashSet();
                        dockerImageTagList.forEach(item -> {
                            manifestSet.addAll(item.getManifests());
                            blobSet.addAll(item.getBlobs());
                        });
                        if (Files.exists(imageManifestsRepositoryPath)) {
                            try (Stream<Path> pathStream = Files.list(imageManifestsRepositoryPath)) {
                                pathStream.forEach(p -> {
                                    try {
                                        RepositoryPath repositoryPath = (RepositoryPath) p;
                                        if (!RepositoryFiles.isChecksum(repositoryPath) && !manifestSet.contains(p.getFileName().toString())) {
                                            artifactManagementService.delete(repositoryPath, true);
                                            resultList.add(successMsg);
                                            log.info("Docker repository [{}] [{}] image [{}] manifest [{}] did not use delete", storageId, repositoryId, path.getFileName().toString(), p.toString());
                                        }
                                    } catch (Exception ex) {
                                        resultList.add(failMsg);
                                        log.error(ExceptionUtils.getStackTrace(ex));
                                    }
                                });
                            }
                        }
                        if (!Files.exists(imageManifestsRepositoryPath) || Files.list(imageManifestsRepositoryPath).count() == 0) {
                            artifactManagementService.delete(imageRepositoryPath, true);
                            resultList.add(successMsg);
                            log.info("Docker repository [{}] [{}] image [{}] not exists manifest delete", storageId, repositoryId, path.getFileName().toString());
                            continue;
                        }
                        if (Files.exists(imageBlobsRepositoryPath)) {
                            try (Stream<Path> pathStream = Files.list(imageBlobsRepositoryPath)) {
                                pathStream.forEach(p -> {
                                    try {
                                        RepositoryPath repositoryPath = (RepositoryPath) p;
                                        if (!RepositoryFiles.isChecksum(repositoryPath) && !blobSet.contains(p.getFileName().toString())) {
                                            artifactManagementService.delete(repositoryPath, true);
                                            resultList.add(successMsg);
                                            log.info("Docker repository [{}] [{}] image [{}] blob [{}] did not use delete", storageId, repositoryId, path.getFileName().toString(), p.toString());
                                        }
                                    } catch (Exception ex) {
                                        resultList.add(failMsg);
                                        log.error(ExceptionUtils.getStackTrace(ex));
                                    }
                                });
                            }
                        }
                        if (!Files.exists(imageBlobsRepositoryPath) || Files.list(imageBlobsRepositoryPath).count() == 0) {
                            artifactManagementService.delete(imageRepositoryPath, true);
                            resultList.add(successMsg);
                            log.info("Docker repository [{}] [{}] image [{}] not exists blobs delete", storageId, repositoryId, path.getFileName().toString());
                            continue;
                        }
                        if (!Files.exists(imageRepositoryPath) || Files.list(imageRepositoryPath).count() == 0) {
                            artifactManagementService.delete(imageRepositoryPath, true);
                            resultList.add(successMsg);
                            log.info("Docker repository [{}] [{}] image [{}] not exists files delete", storageId, repositoryId, path.getFileName().toString());
                        }
                    } catch (Exception ex) {
                        resultList.add(failMsg);
                        log.error("Clean docker artifact job repository [{}] [{}] [{}] error {}", storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
                    }
                }
                long success = resultList.stream().filter(successMsg::equals).count(), fail = resultList.stream().filter(failMsg::equals).count(), other = 0;
                other = resultList.size() - success - fail;
                log.info("Clean docker artifact job repository [{}] [{}] finished success [{}] fail [{}] other [{}]",
                        storageId, repositoryId, success, fail, other);
            } catch (Exception e) {
                log.error("Clean docker artifact job repository [{}] [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
            }
        } else {
            log.warn("Docker仓库清理任务不生效，请重新配置!");
        }
        log.info("Clean docker artifact job repository [{}] [{}] end", storageId, repositoryId);
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

    /**
     * 获取仓库下的目录列表
     *
     * @param repositoryPath 仓库路径
     * @param excludeList    要排除的列表
     * @return 目录列表
     * @throws IOException 异常
     */
    private List<Path> getDirectory(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        List<Path> directoryList;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            directoryList = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}]", p);
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
        return directoryList;
    }

    /**
     * 处理manifest和blob文件
     *
     * @param repositoryPath             路径
     * @param manifestRootRepositoryPath manifestRootRepositoryPath
     * @param blobsRootRepositoryPath    blobsRootRepositoryPath
     * @param dockerImageTagList         dockerImageTagList
     * @return DockerImageTag
     * @throws IOException io异常
     */
    public DockerImageTag handlerDockerImageTag(RepositoryPath repositoryPath, RepositoryPath manifestRootRepositoryPath, RepositoryPath blobsRootRepositoryPath, List<DockerImageTag> dockerImageTagList) throws IOException {
        if (!Files.isDirectory(repositoryPath)) {
            return null;
        }
        //当前版本下manifest文件信息
        List<Path> pathList = Files.list(repositoryPath).filter(f -> !Files.isDirectory(f) && !f.getFileName().toString().endsWith(".sha256") && f.getFileName().toString().startsWith("sha256")).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(pathList)) {
            log.warn("Clean docker artifact job repository [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath);
            return null;
        }
        RepositoryPath manifestRepositoryPath = manifestRootRepositoryPath.resolve(pathList.get(0).getFileName());
        List<ImageManifest> imageManifestList = getImageManifests(manifestRepositoryPath);
        if (CollectionUtils.isEmpty(imageManifestList)) {
            return null;
        }
        Set<String> manifests = Sets.newLinkedHashSet(), blobs = Sets.newLinkedHashSet();
        DockerImageTag dockerImage = DockerImageTag.builder().manifests(manifests).blobs(blobs).build();
        for (ImageManifest imageManifest : imageManifestList) {
            manifests.add(imageManifest.getDigest());
            if (Objects.nonNull(imageManifest.getConfig())) {
                if (!Files.exists(blobsRootRepositoryPath.resolve(imageManifest.getConfig().getDigest()))) {
                    log.warn("Clean docker artifact job repository [{}] [{}] [{}] config blobs [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getFileName().toString(), imageManifest.getConfig().getDigest());
                    return null;
                }
                blobs.add(imageManifest.getConfig().getDigest());
            }
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                for (LayerManifest layerManifest : imageManifest.getLayers()) {
                    if (!Files.exists(blobsRootRepositoryPath.resolve(layerManifest.getDigest()))) {
                        log.warn("Clean docker artifact job repository [{}] [{}] [{}] blobs [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getFileName().toString(), layerManifest.getDigest());
                        return null;
                    }
                    blobs.add(layerManifest.getDigest());
                }
            }
        }
        dockerImageTagList.add(dockerImage);
        return dockerImage;
    }

    public List<ImageManifest> getImageManifests(RepositoryPath repositoryPath) throws IOException {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Clean docker artifact job repository [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.toString());
            return null;
        }
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        String imageName = dockerArtifactCoordinates.getName();
        List<ImageManifest> imageManifestList = Lists.newArrayList();
        String manifestString = Files.readString(repositoryPath);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        RepositoryPath manifestRootRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName + "/manifest");
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            //多架构镜像
            ImageManifest itemImageManifest = null;
            for (Manifests manifests : imageManifest.getManifests()) {
                RepositoryPath manifestPath = manifestRootRepositoryPath.resolve(manifests.getDigest());
                if (!Files.exists(manifestPath)) {
                    log.warn("Clean docker artifact job repository [{}] [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName, dockerArtifactCoordinates.getLayers());
                    return null;
                }
                manifestString = Files.readString(manifestPath);
                itemImageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                itemImageManifest.setDigest(manifests.getDigest());
                imageManifestList.add(itemImageManifest);
            }
        }
        if (!Files.exists(manifestRootRepositoryPath.resolve(dockerArtifactCoordinates.getLayers()))) {
            log.warn("Clean docker artifact job repository [{}] [{}] [{}] manifest [{}] not exists, The image is damaged and will be deleted", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), imageName, dockerArtifactCoordinates.getLayers());
            return null;
        }
        imageManifest.setDigest(dockerArtifactCoordinates.getLayers());
        imageManifestList.add(imageManifest);
        return imageManifestList;
    }
}

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
class DockerImageTag {

    /**
     * manifests
     */
    private Set<String> manifests;

    /**
     * blobs
     */
    private Set<String> blobs;
}