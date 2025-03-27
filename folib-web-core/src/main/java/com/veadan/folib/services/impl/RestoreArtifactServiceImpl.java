package com.veadan.folib.services.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.metadata.indexer.RpmRepoIndexer;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.FutureTask;

/**
 * @author leipenghui
 * @date 2025/3/25
 **/
@Slf4j
@Service
public class RestoreArtifactServiceImpl implements RestoreArtifactService {

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private ArtifactManagementService artifactManagementService;

    @Inject
    @Lazy
    private PromotionUtil promotionUtil;

    @Inject
    @Lazy
    private ArtifactMetadataService artifactMetadataService;

    @Inject
    @Lazy
    private ArtifactService artifactService;

    @Inject
    @Lazy
    private ArtifactWebService artifactWebService;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    @Lazy
    private ArtifactIdGroupService artifactIdGroupService;

    @Inject
    @Lazy
    private ArtifactTagService artifactTagService;

    @Inject
    @Lazy
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void restoreArtifact(RepositoryPath repositoryPath) throws Exception {
        if (!RepositoryFiles.isTrash(repositoryPath) || Files.isSameFile(repositoryPath, repositoryPath.getRoot())) {
            return;
        }
        restoreArtifacts(repositoryPath, 6);
        Repository repository = repositoryPath.getRepository();
        RepositoryFiles.delete(repositoryPath, true);
        if (ProductTypeEnum.Rpm.getFoLibraryName().equals(repository.getLayout())) {
            RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
            rpmRepoIndexer.indexWriter(repository);
        }
    }

    /**
     * 处理待还原制品
     *
     * @param trashRepositoryPath trashRepositoryPath
     * @param batch               每批数量
     */
    private void restoreArtifacts(RepositoryPath trashRepositoryPath, Integer batch) throws Exception {
        ThreadPoolTaskExecutor threadPoolTaskExecutor = getThreadPoolTaskExecutor();
        try {
            Repository repository = trashRepositoryPath.getRepository();
            List<RepositoryPath> trashRepositoryPaths = RepositoryPathUtil.getTrashPaths(trashRepositoryPath);
            List<List<RepositoryPath>> fileLists = Lists.partition(trashRepositoryPaths, batch);
            List<FutureTask<String>> futureTaskList = Lists.newArrayList();
            FutureTask<String> futureTask = null;
            for (List<RepositoryPath> fileList : fileLists) {
                futureTask = new FutureTask<String>(() -> {
                    String tempArtifactPath, artifactPath;
                    RepositoryPath targetRepositoryPath;
                    for (RepositoryPath restoreRepositoryPath : fileList) {
                        try {
                            tempArtifactPath = RepositoryFiles.relativizePath(restoreRepositoryPath);
                            artifactPath = tempArtifactPath.replace(LayoutFileSystem.TRASH + File.separator, "");
                            targetRepositoryPath = repositoryPathResolver.resolve(restoreRepositoryPath.getStorageId(), restoreRepositoryPath.getRepositoryId(), artifactPath);
                            doRestore(repository, restoreRepositoryPath, targetRepositoryPath, artifactPath);
                        } catch (Exception ex) {
                            log.error("RestoreArtifacts path [{}] error [{}]", restoreRepositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                        }
                    }
                    return "success";
                });
                futureTaskList.add(futureTask);
                threadPoolTaskExecutor.submit(futureTask);
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            log.info("RestoreArtifacts storageId [{}] repositoryId [{}] artifactPath [{}] is finished", trashRepositoryPath.getStorageId(), trashRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(trashRepositoryPath));
        } finally {
            threadPoolTaskExecutor.shutdown();
            threadPoolTaskExecutor.destroy();
        }
    }

    private void doRestore(Repository repository, RepositoryPath restoreRepositoryPath, RepositoryPath targetRepositoryPath, String artifactPath) throws Exception {
        if (Files.isDirectory(restoreRepositoryPath)) {
            String metadata = artifactComponent.getCacheArtifactMetadata(restoreRepositoryPath);
            if (StringUtils.isBlank(metadata)) {
                return;
            }
            RepositoryPath sourceArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(restoreRepositoryPath);
            if (Files.exists(sourceArtifactMetadataPath)) {
                //移动元数据文件
                RepositoryPath targetArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(targetRepositoryPath);
                Files.createDirectories(targetArtifactMetadataPath.getParent());
                Files.move(sourceArtifactMetadataPath.getTarget(), targetArtifactMetadataPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
            } else {
                //缓存元数据文件
                artifactComponent.cacheArtifactMetadata(targetRepositoryPath, metadata);
            }
            return;
        }
        if (!RepositoryFiles.isArtifact(targetRepositoryPath)) {
            log.info("RestoreArtifacts path [{}] not is a artifact", targetRepositoryPath.toString());
            return;
        }
        //若目标存在脏数据，强制删除
        artifactWebService.doForceDelete(targetRepositoryPath);
        //从回收站内获取需要还原的元数据信息
        String metadata = artifactComponent.getCacheArtifactMetadata(restoreRepositoryPath);
        moveFile(restoreRepositoryPath, targetRepositoryPath, metadata);
        if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
            try {
                artifactMetadataService.rebuildMetadata(targetRepositoryPath.getStorageId(), targetRepositoryPath.getRepositoryId(), artifactPath);
            } catch (Exception ex) {
                log.error("RestoreArtifacts rebuildMetadata path [{}] error [{}]", targetRepositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    public void moveFile(RepositoryPath restoreRepositoryPath, RepositoryPath targetRepositoryPath, String metadata) throws IOException {
        boolean exist = Files.exists(targetRepositoryPath);
        Files.createDirectories(targetRepositoryPath.getParent());
        Map<String, String> digestMap = Maps.newHashMap();
        restoreRepositoryPath.getFileSystem().provider().resolveChecksumPathMap(restoreRepositoryPath).forEach((key, value) -> {
            if (Files.exists(value)) {
                try {
                    digestMap.put(key, Files.readString(value));
                } catch (FileAlreadyExistsException e) {
                    //destination file already exists
                } catch (Exception ex) {
                    log.warn("Copy checksumPath [{}] [{}] [{}] error [{}]", restoreRepositoryPath.getStorageId(), restoreRepositoryPath.getRepositoryId(), restoreRepositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                }
            }
        });
        long startTime = System.currentTimeMillis();
        Files.move(restoreRepositoryPath.getTarget(), targetRepositoryPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
        log.info("Move file [{}] take time [{}] ms", restoreRepositoryPath, System.currentTimeMillis() - startTime);
        if (RepositoryFiles.isArtifact(targetRepositoryPath)) {
            //生成图库索引
            buildArtifact(targetRepositoryPath, digestMap, metadata);
            Artifact artifact = targetRepositoryPath.getArtifactEntry();
            if (Objects.nonNull(artifact)) {
                if (StringUtils.isNotBlank(artifact.getMetadata())) {
                    RepositoryPath sourceArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(restoreRepositoryPath);
                    if (Files.exists(sourceArtifactMetadataPath)) {
                        //移动元数据文件
                        RepositoryPath targetArtifactMetadataPath = artifactComponent.getCacheArtifactMetadataPath(targetRepositoryPath);
                        Files.createDirectories(targetArtifactMetadataPath.getParent());
                        Files.move(sourceArtifactMetadataPath.getTarget(), targetArtifactMetadataPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
                    } else {
                        //缓存元数据文件
                        artifactComponent.cacheArtifactMetadata(targetRepositoryPath, artifact.getMetadata());
                    }
                }
                if (MapUtils.isNotEmpty(artifact.getChecksums())) {
                    artifact.getChecksums().forEach((key, value) -> {
                        try {
                            String checksumExtension = ".".concat(key.toLowerCase().replaceAll("-", ""));
                            RepositoryPath checksumRepositoryPath = restoreRepositoryPath.resolveSibling(restoreRepositoryPath.getFileName().toString() + checksumExtension);
                            if (Files.exists(checksumRepositoryPath)) {
                                try {
                                    RepositoryPath checksumTargetRepositoryPath = targetRepositoryPath.resolveSibling(restoreRepositoryPath.getFileName().toString() + checksumExtension);
                                    Files.move(checksumRepositoryPath.getTarget(), checksumTargetRepositoryPath.getTarget(), StandardCopyOption.REPLACE_EXISTING);
                                } catch (FileAlreadyExistsException e) {
                                    //destination file already exists
                                } catch (Exception ex) {
                                    log.warn("Copy checksumPath [{}] error [{}]", checksumRepositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                                }
                            } else {
                                MessageDigestUtils.writeChecksum(targetRepositoryPath, checksumExtension, value);
                            }
                        } catch (IOException ioEx) {
                            log.warn(ExceptionUtils.getStackTrace(ioEx));
                        }
                    });
                }
            }
            if (exist) {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(targetRepositoryPath);
            } else {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(targetRepositoryPath);
            }
        } else if (RepositoryFiles.isMetadata(targetRepositoryPath)) {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(targetRepositoryPath);
        }
    }

    public void buildArtifact(RepositoryPath targetRepositoryPath, Map<String, String> digestMap, String metadata) throws IOException {
        Artifact targetArtifact = targetRepositoryPath.getArtifactEntry();
        if (targetArtifact == null) {
            targetArtifact = artifactService.provideArtifact(targetRepositoryPath);
            if (targetArtifact == null) {
                return;
            }
        }
        targetRepositoryPath.setArtifact(targetArtifact);
        String username = UserUtils.getUsername();
        LocalDateTime now = LocalDateTimeInstance.now();
        if (Objects.isNull(targetArtifact.getNativeId())) {
            targetArtifact.setCreated(now);
            targetArtifact.setCreatedBy(username);
        }
        targetArtifact.setLastUpdated(now);
        targetArtifact.setLastUsed(now);
        targetArtifact.setUpdatedBy(username);
        Repository repository = targetRepositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(targetRepositoryPath);
        targetArtifact.setSizeInBytes(Files.size(targetRepositoryPath));
        if (MapUtils.isNotEmpty(digestMap)) {
            targetArtifact.setChecksums(digestMap);
        }
        targetArtifact.setMetadata(metadata);
        targetArtifact.setArtifactFileExists(Boolean.TRUE);
        targetArtifact.setEnabled(Boolean.TRUE);
//        ArtifactArchiveListing sourceArtifactArchiveListing = null;
//        if (Objects.nonNull(sourceArtifactArchiveListing)) {
//            ArtifactArchiveListing artifactArchiveListing = targetArtifact.getArtifactArchiveListing();
//            artifactArchiveListing.setFilenames(sourceArtifactArchiveListing.getFilenames());
//        }
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactGroupWithTag(storage.getId(),
                repository.getId(),
                coordinates.getId(),
                Optional.of(lastVersionTag))
                .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                        repository.getId(),
                        coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, targetArtifact);
        log.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());
        try {
            artifactIdGroupRepository.saveOrUpdate(artifactGroup);
        } catch (Exception ex) {
            String realMessage = CommonUtils.getRealMessage(ex);
            log.warn("[{}] [{}] merge group error [{}]",
                    this.getClass().getSimpleName(), targetRepositoryPath, realMessage);
            if (CommonUtils.catchException(realMessage)) {
                log.warn("[{}] [{}] merge group catch error",
                        this.getClass().getSimpleName(), targetRepositoryPath);
                return;
            }
            throw ex;
        }
    }

    private ThreadPoolTaskExecutor getThreadPoolTaskExecutor() {
        int availableCores = 16;
        return commonComponent.buildThreadPoolTaskExecutor("asyncRestoreArtifact", availableCores, availableCores);
    }

}
