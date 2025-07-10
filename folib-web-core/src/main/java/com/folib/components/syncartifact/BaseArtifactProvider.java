/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import com.google.common.collect.Lists;
import com.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.folib.constant.GlobalConstants;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.entity.MigrateInfo;
import com.folib.enums.MigrateStatusEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.MigrateInfoService;
import com.folib.storage.repository.Repository;
import com.folib.util.UriUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.util.StopWatch;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.CountDownLatch;
import java.util.stream.Stream;

/**
 * @author veadan
 * @since 2025-01-20 14:54
 */
@Slf4j
public abstract class BaseArtifactProvider implements SyncArtifactProvider {

    private final SyncUtils syncUtils;

    private final MigrateInfoService migrateInfoService;


    public BaseArtifactProvider(SyncUtils syncUtils, MigrateInfoService migrateInfoService) {
        this.syncUtils = syncUtils;
        this.migrateInfoService = migrateInfoService;
    }

    @Override
    public abstract void register();

    public abstract String getLayout();

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {
        String dirPath = syncPackageIndex(syncArtifactForm);
        if (StringUtils.isBlank(dirPath)) {
            return;
        }
        handlerPath(dirPath, syncArtifactForm);
    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {
        try {
            MigrateInfo repository = migrateInfoService.getByMigrateIdAndRepoInfo(syncArtifactForm.getMigrateId(), syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            int total = repository.getTotalArtifact() == null ? 0 : repository.getTotalArtifact();
            syncArtifactForm.setTotalArtifact(total);
            if (MigrateStatusEnum.QUEUING.getStatus() == repository.getSyncStatus() && repository.getIndexFinish() == 0) {
                migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.FETCHING_INDEX.getStatus());
                String dirPath = syncPackageIndex(syncArtifactForm);
                if (dirPath == null) {
                    log.info("同步索引失败,请稍后重试");
                    migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.INDEX_FAILED.getStatus());
                    return;
                }
                repository.setSyncDirPath(dirPath);
                repository.setTotalArtifact(syncArtifactForm.getTotalArtifact());
            }
            repository.setSyncStatus(MigrateStatusEnum.SYNCING_ARTIFACT.getStatus());
            // 更新状态
            migrateInfoService.updateById(repository);
            syncUtils.resetArtifact(syncArtifactForm.getStoreAndRepo());
            syncUtils.resetDirectoryCount(syncArtifactForm.getStoreAndRepo());
            syncUtils.setIndex(syncArtifactForm.getStoreAndRepo(),total);
            String path = repository.getSyncDirPath();
            if (syncArtifactForm.getSyncMeta() == 1) {
                JfrogPropertySyncer syncer = new JfrogPropertySyncer(syncArtifactForm.getApiUrl(), syncArtifactForm.getUsername(), syncArtifactForm.getPassword());
                syncArtifactForm.setSyncer(syncer);
            }
            handlerPath(path, syncArtifactForm);
            migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.COMPLETED.getStatus());
        } finally {
            if (Objects.nonNull(syncArtifactForm.getSyncer())) {
                syncArtifactForm.getSyncer().close();
            }
        }
    }

    public abstract boolean isArtifact(String url);


    /**
     * @param currentUrl 当前获取的url
     * @param preUrl     上一级url
     * @return 为上一级的子目录则为true
     */
    public boolean isSubDirectory(String currentUrl, String preUrl) {
        return currentUrl.contains(preUrl) && !currentUrl.equals(preUrl) && currentUrl.endsWith(GlobalConstants.SEPARATOR);
    }


    protected void findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, File file, FileWriter writer) {
        try {
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            syncUtils.artifactComponent.parseLinksStreaming(repository, url, absUrl -> {
                try {
                    absUrl = UriUtils.decode(absUrl);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                    return;
                }
                if (isSubDirectory(absUrl, url)) {
                    String path = absUrl.substring(rootUrl.length());
                    try {
                        writer.write(path + "\n");
                        writer.flush();
                    } catch (IOException e) {
                        log.error("写索引文件{}异常{}", absUrl, e.getMessage(), e);
                    }
                    return;
                }
                if (isArtifact(absUrl)) {
                    absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
                    syncUtils.storeContent(absUrl, file.getParent() + "/artifact");
                    syncUtils.indexIncrease(repository.getStorageIdAndRepositoryId());
                }
            });
        } catch (Exception e) {
            log.error("【{}】包索引同步制品，错误 [{}]", getLayout(), ExceptionUtils.getStackTrace(e));
        }
    }


    private String syncPackageIndex(SyncArtifactForm syncArtifactForm) {
        try {
            StopWatch sw = new StopWatch();
            sw.start();
            syncUtils.resetIndex(syncArtifactForm.getStoreAndRepo());
            Repository repository = syncUtils.validRepo(syncArtifactForm);
            if (Objects.isNull(repository)) {
                return null;
            }
            String baseUri = syncUtils.getBaseUri();
            if (baseUri.endsWith(GlobalConstants.SEPARATOR)) {
                baseUri = baseUri.substring(0, baseUri.lastIndexOf(GlobalConstants.SEPARATOR));
            }
            String repositoryBaseUri = String.format("%s/storages/%s/%s", baseUri, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            String remoteUrl = repository.getRemoteRepository().getUrl();
            if (remoteUrl.endsWith(GlobalConstants.SEPARATOR)) {
                remoteUrl = remoteUrl.substring(0, remoteUrl.lastIndexOf(GlobalConstants.SEPARATOR));
            }
            if (syncArtifactForm.getSyncMeta() == 1 && syncArtifactForm.getSyncer() == null) {
                String apiUrl = remoteUrl.substring(0, remoteUrl.indexOf(repository.getId()));
                JfrogPropertySyncer syncer = new JfrogPropertySyncer(apiUrl, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
                syncArtifactForm.setSyncer(syncer);
            }
            String rootUrl = remoteUrl;
            if (StringUtils.isNotBlank(syncArtifactForm.getBrowseUrl())) {
                rootUrl = syncArtifactForm.getBrowseUrl();
            }
            if (rootUrl.endsWith(GlobalConstants.SEPARATOR)) {
                rootUrl = rootUrl.substring(0, rootUrl.lastIndexOf(GlobalConstants.SEPARATOR));
            }
            String dirPath = syncUtils.getTempPath() + File.separator + "syncArtifact" + File.separator + syncArtifactForm.getStorageId() + File.separator + syncArtifactForm.getRepositoryId();
            log.info("【{}】包索引同步， 仓库地址 [{}] 存放爬取信息的目录 [{}]", getLayout(), repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("【{}】包索引同步存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", getLayout(), dirPath, flag);
            } else {
                FileUtil.clean(dir);
            }
            Integer sleepMillis = null;
            if (Objects.nonNull(syncArtifactForm.getSleepMillis())) {
                sleepMillis = syncArtifactForm.getSleepMillis();
            }
            int level = 0;
            File rootFile = getLevelFile(dir, level);
            if (!rootFile.exists()) {
                boolean flag = rootFile.createNewFile();
                log.info("【{}】包索引同步存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", getLayout(), rootFile.getAbsolutePath(), flag);
                try (FileWriter writer = new FileWriter(rootFile)) {
                    writer.write("/\n");
                    writer.flush();
                }
            }
            File urlFile;
            while ((urlFile = getLevelFile(dir, level)).exists()) {
                level++;
                log.info("开始检索{}下的目录", urlFile);
                boolean fileEmpty = true;
                File subFile = getLevelFile(dir, level);
                try (Scanner scanner = new Scanner(urlFile);
                     FileWriter writer = new FileWriter(subFile)) {
                    while (scanner.hasNext()) {
                        String line = scanner.nextLine();
                        if (StringUtils.isNotBlank(line)) {
                            if (!line.startsWith(GlobalConstants.SEPARATOR)) {
                                line = GlobalConstants.SEPARATOR + line;
                            }
                            fileEmpty = false;
                            String url = rootUrl + line;
                            findSubUrl(repository, rootUrl, url, remoteUrl, sleepMillis, subFile, writer);
                        }
                    }
                } catch (IOException e) {
                    log.error("【{}】包索引同步错误 [{}]", getLayout(), ExceptionUtils.getStackTrace(e));
                    return null;
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            sw.stop();
            int total = syncUtils.getIndexCount(syncArtifactForm.getStoreAndRepo());
            log.info("【{}】包索引同步完成耗时 【{}】秒, 同步制品总个数 [{}]", getLayout(), sw.getTotalTimeSeconds(), total);
            syncArtifactForm.setTotalArtifact(total);
            return dirPath;
        } catch (Exception e) {
            log.error("【{}】包索引同步，错误 [{}]", getLayout(), ExceptionUtils.getStackTrace(e));
            return null;
        }
    }

    private File getLevelFile(File dir, int level) {
        return new File(dir.getAbsolutePath() + File.separator + "level_" + level + ".txt");
    }

    private void handlerPath(String dirPath, SyncArtifactForm syncArtifactForm) {
        StopWatch swTotal = new StopWatch();
        swTotal.start();
        Path path = Path.of(dirPath + "/artifact");
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        int availableCores = syncArtifactForm.getMaxThreadNum() == null ? syncUtils.getDefaultThreadNums() : syncArtifactForm.getMaxThreadNum();
        ThreadPoolTaskExecutor threadPoolTaskExecutor = syncUtils.createThreadPool("browseSync:" + syncArtifactForm.getStoreAndRepo(), availableCores, availableCores);
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
            pathStream.forEach(item -> {
                StopWatch swBatch = new StopWatch();
                swBatch.start();
                String currentLine = "";
                long lines = 0;
                try {
                    List<String> pathList = Lists.newArrayList();
                    try (LineIterator lineIterator = FileUtils.lineIterator(item.toFile(), "UTF-8")) {
                        while (lineIterator.hasNext()) {
                            try {
                                lines++;
                                currentLine = lineIterator.nextLine();
                                if (StringUtils.isBlank(currentLine)) {
                                    continue;
                                }
                                pathList.add(currentLine);
                                if (pathList.size() == finalBatch) {
                                    batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                                }
                            } catch (Exception ex) {
                                log.error(ExceptionUtils.getStackTrace(ex));
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] lines [{}] error [{}] ms", item.toString(), lines, ExceptionUtils.getStackTrace(ex));
                }
                swBatch.stop();
                log.info("Handle path [{}] lines [{}] finished take time [{}] s", item, lines, swBatch.getTotalTimeSeconds());
            });
        } catch (Exception ex) {
            log.error("Error [{}]", ExceptionUtils.getStackTrace(ex));
        } finally {
            threadPoolTaskExecutor.shutdown();
        }
        int total = syncUtils.getArtifactCount(syncArtifactForm.getStoreAndRepo());
        syncArtifactForm.setSyncMount(total);
        swTotal.stop();
        log.info("【{}】包同步完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个制品，耗时 [{}] 秒", getLayout(), syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), total, swTotal.getTotalTimeSeconds());
        handlerDirectoryMetadata(dirPath, syncArtifactForm);
    }

    private void handlerDirectoryMetadata(String dirPath, SyncArtifactForm syncArtifactForm) {
        StopWatch swTotal = new StopWatch();
        swTotal.start();
        Path path = Path.of(dirPath);
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        int availableCores = syncArtifactForm.getMaxThreadNum() == null ? syncUtils.getDefaultThreadNums() : syncArtifactForm.getMaxThreadNum();
        String levelPrefix = "level_";
        ThreadPoolTaskExecutor threadPoolTaskExecutor = syncUtils.createThreadPool("browseSync:" + syncArtifactForm.getStoreAndRepo(), availableCores, availableCores);
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
            pathStream.filter(item -> Files.isRegularFile(item) && item.getFileName().toString().startsWith(levelPrefix)).forEach(item -> {
                StopWatch swBatch = new StopWatch();
                swBatch.start();
                String currentLine = "";
                long lines = 0;
                try {
                    List<String> pathList = Lists.newArrayList();
                    try (LineIterator lineIterator = FileUtils.lineIterator(item.toFile(), "UTF-8")) {
                        while (lineIterator.hasNext()) {
                            try {
                                lines++;
                                currentLine = lineIterator.nextLine();
                                if (StringUtils.isBlank(currentLine)) {
                                    continue;
                                }
                                currentLine = StringUtils.removeEnd(StringUtils.removeStart(currentLine, GlobalConstants.SEPARATOR), GlobalConstants.SEPARATOR);
                                if (StringUtils.isBlank(currentLine)) {
                                    continue;
                                }
                                pathList.add(currentLine);
                                if (pathList.size() == finalBatch) {
                                    batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                                }
                            } catch (Exception ex) {
                                log.error(ExceptionUtils.getStackTrace(ex));
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] lines [{}] error [{}] ms", item.toString(), lines, ExceptionUtils.getStackTrace(ex));
                }
                swBatch.stop();
                log.info("Handle path [{}] lines [{}] finished take time [{}] s", item, lines, swBatch.getTotalTimeSeconds());
            });
        } catch (Exception ex) {
            log.error("Error [{}]", ExceptionUtils.getStackTrace(ex));
        } finally {
            threadPoolTaskExecutor.shutdown();
        }
        int total = syncUtils.getDirectoryCount(syncArtifactForm.getStoreAndRepo());
        swTotal.stop();
        log.info("同步目录元数据完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个目录元数据，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), total, swTotal.getTotalTimeSeconds());
    }

    protected void batchDownload(Path path, SyncArtifactForm form, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        CountDownLatch latch = new CountDownLatch(artifactPathList.size());
        for (String artifactPath : artifactPathList) {
            threadPoolTaskExecutor.submit(() -> {
                this.downloadByPath(path, artifactPath, form);
                latch.countDown();
            });
        }
        try {
            latch.await();
        } catch (InterruptedException e) {
            log.info("异常中断");
        }
        //清理
        artifactPathList.clear();
    }

    public void downloadByPath(Path path, String artifactPath, SyncArtifactForm form) {
        try {
            String levelPrefix = "level_", fileName = path.getFileName().toString();
            String storageId = form.getStorageId();
            String repositoryId = form.getRepositoryId();
            if (StringUtils.isNotBlank(artifactPath)) {
                //制品
                RepositoryPath repositoryPath = syncUtils.resolve(storageId, repositoryId, artifactPath);
                if (fileName.startsWith(levelPrefix) && (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath))) {
                    //是目录的索引文件，并且在同步目录元数据时，该目录不存在，跳过处理
                    return;
                }
                if (Files.exists(repositoryPath) && Files.isDirectory(repositoryPath)) {
                    //目录
                    syncUtils.directoryIncrease(form.getStoreAndRepo());
                    JfrogPropertySyncer syncer = form.getSyncer();
                    if (syncer != null) {
                        String properties = syncer.getPropertiesByKeyAndPath(repositoryId, artifactPath);
                        if (properties != null) {
                            syncUtils.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
                        }
                    }
                    return;
                }
                if (Files.exists(repositoryPath)) {
                    syncUtils.artifactIncrease(form.getStoreAndRepo());
                    log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                    return;
                }
                syncUtils.resolvePath(storageId, repositoryId, artifactPath);
                if (Files.exists(repositoryPath)) {
                    // 添加成功 计数
                    syncUtils.artifactIncrease(form.getStoreAndRepo());
                    JfrogPropertySyncer syncer = form.getSyncer();
                    if (syncer != null) {
                        String properties = syncer.getPropertiesByKeyAndPath(repositoryId, artifactPath);
                        if (properties != null) {
                            syncUtils.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
                        }
                    }
                }
            }
        } catch (Exception ex) {
            log.error("Batch download artifactPath [{}] error [{}]", artifactPath, ExceptionUtils.getStackTrace(ex));
        }
    }


}
