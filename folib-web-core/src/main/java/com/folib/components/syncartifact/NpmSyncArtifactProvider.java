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
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.DistributedCounterComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.common.CommonComponent;
import com.folib.components.files.FilesCommonComponent;
import com.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.entity.MigrateInfo;
import com.folib.enums.ArtifactSyncTypeEnum;
import com.folib.enums.MigrateStatusEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactWebService;
import com.folib.services.JfrogMigrateService;
import com.folib.services.MigrateInfoService;
import com.folib.services.NpmService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.UriUtils;
import com.folib.utils.NpmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class NpmSyncArtifactProvider implements SyncArtifactProvider {

    /**
     * 计数
     */
    private static final AtomicLong COUNT = new AtomicLong(0);

    /**
     * 计数
     */
    private static final ThreadLocal<Integer> THREAD_LOCAL_ARTIFACT = ThreadLocal.withInitial(() -> 0);

    /**
     * 计数
     */
    private static final ThreadLocal<Integer> THREAD_LOCAL_PACKAGE = ThreadLocal.withInitial(() -> 0);

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private FilesCommonComponent filesCommonComponent;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    private NpmService npmService;

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private DistributedCacheComponent distributedCacheComponent;
    @Resource
    private MigrateInfoService migrateInfoService;
    @Resource
    private ArtifactWebService artifactWebService;


    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.NPM.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.NPM.getType());
    }

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

    private String syncPackageIndex(SyncArtifactForm syncArtifactForm) {
        try {
            long startTime = System.currentTimeMillis();
            distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + syncArtifactForm.getStoreAndRepo()).set(0);
            Repository repository = configurationManager.getRepository(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            if (Objects.isNull(repository)) {
                throw new RuntimeException(String.format("存储空间 [%s] 所属仓库 [%s}] 不存在", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId()));
            }
            if (!RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repository.getType())) {
                throw new RuntimeException(String.format("存储空间 [%s] 所属仓库 [%s}] 不是代理库", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId()));
            }
            String separator = "/";
            String baseUri = configurationManager.getBaseUri().toString();
            if (baseUri.endsWith(separator)) {
                baseUri = baseUri.substring(0, baseUri.lastIndexOf(separator));
            }
            String repositoryBaseUri = String.format("%s/storages/%s/%s", baseUri, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            String remoteUrl = repository.getRemoteRepository().getUrl();
            if (remoteUrl.endsWith(separator)) {
                remoteUrl = remoteUrl.substring(0, remoteUrl.lastIndexOf(separator));
            }
            if(syncArtifactForm.getSyncMeta()==1&&syncArtifactForm.getSyncer()==null){
                String apiUrl=remoteUrl.substring(0,remoteUrl.indexOf(repository.getId()));
                JfrogPropertySyncer syncer = new JfrogPropertySyncer(apiUrl,repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
                syncArtifactForm.setSyncer(syncer);
            }
            String rootUrl = remoteUrl;
            if (StringUtils.isNotBlank(syncArtifactForm.getBrowseUrl())) {
                rootUrl = syncArtifactForm.getBrowseUrl();
            }
            if (rootUrl.endsWith(separator)) {
                rootUrl = rootUrl.substring(0, rootUrl.lastIndexOf(separator));
            }
            String dirPath = tempPath + File.separator + "syncArtifact" + File.separator + syncArtifactForm.getStorageId() + File.separator + syncArtifactForm.getRepositoryId();
            log.info("RepositoryBaseUri [{}] npm存放爬取信息的目录 [{}]", repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("Npm存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", dirPath, flag);
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
                log.info("Npm存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", rootFile.getAbsolutePath(), flag);
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
                            if (!line.startsWith(separator)) {
                                line = separator + line;
                            }
                            fileEmpty = false;
                            String url = rootUrl + line;
                            if (!findSubUrl(repository, rootUrl, url, remoteUrl, sleepMillis, syncArtifactForm.getDom(), subFile, writer, repositoryBaseUri)) {
                            }
                        }
                    }
                } catch (IOException e) {
                    log.error("Npm包索引同步制品错误 [{}]", ExceptionUtils.getStackTrace(e));
                    return null;
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            syncArtifactForm.setTotalArtifact((int) distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + repository.getStorageIdAndRepositoryId()).get());
            log.info("Npm包索引同步完成，耗时 [{}] ms, 同步包总个数 [{}] 同步制品总个数 [{}]", System.currentTimeMillis() - startTime, THREAD_LOCAL_PACKAGE.get(), THREAD_LOCAL_ARTIFACT.get());
            return dirPath;
        } catch (Exception e) {
            log.error("Npm包索引同步制品，错误 [{}]", ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL_ARTIFACT.remove();
            THREAD_LOCAL_PACKAGE.remove();
        }
        return "";
    }

    /**
     * 获取文件
     *
     * @param dir   目录
     * @param level 等级
     * @return 文件
     */
    private File getLevelFile(File dir, int level) {
        return new File(dir.getAbsolutePath() + File.separator + "level_" + level + ".txt");
    }

    /**
     * 查询子url
     *
     * @param rootUrl           rootUrl
     * @param url               当前url
     * @param remoteUrl         remoteUrl
     * @param sleepMillis       睡眠毫秒数
     * @param dom               页面元素
     * @param file              文件
     * @param writer            writer
     * @param repositoryBaseUri repositoryBaseUri
     */
    private boolean findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, String dom, File file, FileWriter writer, String repositoryBaseUri) {
        try {
            if (isSuffix(url)) {
                return true;
            }
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            String storageId = repository.getStorage().getId(), repositoryId = repository.getId(), separator = "/", npmSeparator = "/-/";
            if (!remoteUrl.endsWith(separator)) {
                remoteUrl = remoteUrl + separator;
            }
            Document doc = artifactComponent.getDocument(repository, url);
            if (Objects.isNull(doc)) {
                log.error("获取文件失败");
                return false;
            }
            Elements links = doc.select(dom);
            NpmCoordinates npmArtifactCoordinates;
            for (Element link : links) {
                String absUrl = link.absUrl("href");
                if (isSuffix(absUrl)) {
                    absUrl = absUrl.replace(remoteUrl, "");
                    npmArtifactCoordinates = resolveNpmArtifactCoordinates(absUrl, npmSeparator);
                    if (Objects.isNull(npmArtifactCoordinates)) {
                        continue;
                    }
                    filesCommonComponent.storeContent(npmArtifactCoordinates.buildPath(), file.getParent() + "/artifact");
                    THREAD_LOCAL_ARTIFACT.set(THREAD_LOCAL_ARTIFACT.get() + 1);
                    distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + repository.getStorageIdAndRepositoryId()).getAndAdd(1);
                } else {
                    // 非子目录
                    if (!absUrl.contains(url) || url.equals(absUrl)) {
                        continue;
                    }
                    absUrl = absUrl.substring(rootUrl.length());
                    if (separator.equals(absUrl)) {
                        continue;
                    }
                    writer.write(absUrl + "\n");
                    writer.flush();
                    if (absUrl.startsWith(separator) || absUrl.endsWith(separator)) {
                        absUrl = UriUtils.decode(StringUtils.removeEnd(StringUtils.removeStart(absUrl, separator), separator));
                        if (absUrl.startsWith(GlobalConstants.AT) && !absUrl.contains(separator)) {
                            continue;
                        }
                        filesCommonComponent.storeContent(absUrl, file.getParent() + "/artifact");
                        THREAD_LOCAL_PACKAGE.set(THREAD_LOCAL_PACKAGE.get() + 1);
                        distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + repository.getStorageIdAndRepositoryId()).getAndAdd(1);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Npm包索引同步制品，错误 [{}]", ExceptionUtils.getStackTrace(e));
            return false;
        }
        return true;
    }

    private NpmCoordinates resolveNpmArtifactCoordinates(String path, String separator) {
        if (StringUtils.isBlank(path) || !path.contains(separator)) {
            return null;
        }
        return NpmCoordinates.parseByResolvePath(path);
    }

    /**
     * 判断后缀
     *
     * @param url url
     * @return true 后缀匹配 false 后缀不匹配
     */
    private boolean isSuffix(String url) {
        return NpmCoordinates.NPM_EXTENSION_LIST.stream().anyMatch(url::endsWith);
    }

    private boolean handlerPath(String dirPath, SyncArtifactForm syncArtifactForm) {
        long allStartTime = System.currentTimeMillis();
        Path path = Path.of(dirPath + "/artifact");
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return  syncArtifactForm.getTotalArtifact()==0;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        COUNT.set(0L);
        int availableCores = syncArtifactForm.getMaxThreadNum() == null ? commonComponent.getAvailableCores() * 2 : syncArtifactForm.getMaxThreadNum();
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browseNpmSync", availableCores, availableCores);
        boolean ispaused = false;
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
            ispaused = pathStream.anyMatch(item -> {
                String currentLine = "";
                long lines = 0, startTime = System.currentTimeMillis();
                boolean flag = true;
                try {
                    List<String> pathList = Lists.newArrayList();
                    try (LineIterator lineIterator = FileUtils.lineIterator(item.toFile(), "UTF-8")) {
                        while (lineIterator.hasNext()) {
                            if ("0".equals(distributedCacheComponent.get(JfrogMigrateService.PAUSED_FLAG_PRE + syncArtifactForm.getStoreAndRepo()))) {
                                //
                                log.info("仓库{}同步任务暂停", syncArtifactForm.getStoreAndRepo());
                                migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.PAUSED.getStatus());
                                distributedCacheComponent.delete(JfrogMigrateService.PAUSED_FLAG_PRE + syncArtifactForm.getStoreAndRepo());
                                return true;
                            }
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
                                flag = false;
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                        }
                    }
                    // 清除已完成的文件
                    if (flag) {
//                        Files.delete(item);
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] lines [{}] error [{}] ms", item.toString(), lines, ExceptionUtils.getStackTrace(ex));
                }
                log.info("Handle path [{}] lines [{}] finished take time [{}] ms", item.toString(), lines, System.currentTimeMillis() - startTime);
                return false;
            });
        } catch (Exception ex) {
            log.error("Error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        syncArtifactForm.setSyncMount((int) COUNT.get());
        log.info("Npm包同步完成，存储空间 [{}]  仓库 [{}] 同步 [{}] 个制品，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), COUNT.get(), System.currentTimeMillis() - allStartTime);
        return !ispaused;
    }

    private void batchDownload(Path path, SyncArtifactForm form, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        String storageId=form.getStorageId();
        String repositoryId=form.getRepositoryId();
        RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        List<List<String>> artifactPathLists = Lists.partition(artifactPathList, 5);
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (List<String> itemArtifactPathList : artifactPathLists) {
            futureTask = new FutureTask<String>(() -> {
                for (String artifactPath : itemArtifactPathList) {
                    try {
                        if (NpmCoordinates.NPM_EXTENSION_LIST.stream().anyMatch(artifactPath::endsWith)) {
                            //制品
                            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                COUNT.incrementAndGet();
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + storageId + ":" + repositoryId).addAndGet(1L);
                                log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                                continue;
                            }
                            artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                COUNT.incrementAndGet();
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + storageId + ":" + repositoryId).addAndGet(1L);
                                JfrogPropertySyncer syncer = form.getSyncer();
                                if (syncer!=null) {
                                    String properties = syncer.getPropertiesByKeyAndPath(repositoryId, artifactPath);
                                    if (properties!=null) {
                                        artifactWebService.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
                                    }
                                }
                            }
                        } else {
                            //索引
                            NpmCoordinates npmArtifactCoordinates = NpmCoordinates.resolveName(null, artifactPath);
                            String packageId = npmArtifactCoordinates.getId();
                            npmService.packageFeed(rootRepositoryPath.getRepository(), packageId,  NpmUtils.getPackageMetadataPath(packageId));
                        }
                    } catch (Exception ex) {
                        log.error("Batch download path [{}] storageId [{}] repositoryId [{}] artifactPath [{}] error [{}]", path.toString(), storageId, repositoryId, artifactPath, ExceptionUtils.getStackTrace(ex));
                    }
                }
                return "success";
            });
            futureTasks.add(futureTask);
            threadPoolTaskExecutor.submit(futureTask);
        }
        futureTasks.forEach(action -> {
            try {
                action.get();
            } catch (Exception e) {
                log.error(e.getMessage(), e);
            }
        });
        //清理
        artifactPathList.clear();
    }


    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {
        try {
            MigrateInfo repository = migrateInfoService.getByMigrateIdAndRepoInfo(syncArtifactForm.getMigrateId(), syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            int total=repository.getTotalArtifact()==null?0:repository.getTotalArtifact();
            syncArtifactForm.setTotalArtifact(total);
            if (MigrateStatusEnum.QUEUING.getStatus() == repository.getSyncStatus() &&repository.getIndexFinish()==0) {
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
            distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + syncArtifactForm.getStoreAndRepo()).set(0);
            String path = repository.getSyncDirPath();
            distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT+syncArtifactForm.getStoreAndRepo()).set(total);
            if (syncArtifactForm.getSyncMeta() == 1) {
                JfrogPropertySyncer syncer = new JfrogPropertySyncer(syncArtifactForm.getApiUrl(), syncArtifactForm.getUsername(), syncArtifactForm.getPassword());
                syncArtifactForm.setSyncer(syncer);
            }
            distributedCacheComponent.put(JfrogMigrateService.PAUSED_FLAG_PRE + syncArtifactForm.getStoreAndRepo(), "1");
            if (handlerPath(path, syncArtifactForm)) {
                migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.COMPLETED.getStatus());
            } else {
                migrateInfoService.updateAndSyncRepoStatus(syncArtifactForm, MigrateStatusEnum.PAUSED.getStatus());
            }
        } finally {
            if(syncArtifactForm.getSyncer()!=null){
                syncArtifactForm.getSyncer().close();
            }
        }

    }


}
