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
import com.folib.components.DistributedCacheComponent;
import com.folib.components.DistributedCounterComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.common.CommonComponent;
import com.folib.components.files.FilesCommonComponent;
import com.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.entity.Dict;
import com.folib.entity.MigrateInfo;
import com.folib.enums.ArtifactSyncTypeEnum;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.MigrateStatusEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactWebService;
import com.folib.services.DictService;
import com.folib.services.JfrogMigrateService;
import com.folib.services.MavenIndexerService;
import com.folib.services.MigrateInfoService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.FutureTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class MavenSyncArtifactProvider implements SyncArtifactProvider {

    /**
     * 计数
     */
    private static final ThreadLocal<Integer> THREAD_LOCAL = ThreadLocal.withInitial(() -> 0);

    /**
     * 匹配后缀
     */
    private final List<String> suffixList = new ArrayList<>(Arrays.asList(".jar,.war,.ear,.zip,.pom,maven-metadata.xml".split(",")));

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private MavenIndexerService mavenIndexerService;

    @Inject
    private DictService dictService;

    @Inject
    private FilesCommonComponent filesCommonComponent;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

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
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.MAVEN.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.MAVEN.getType());
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
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        String storageId = syncArtifactForm.getStorageId(), repositoryId = syncArtifactForm.getRepositoryId();
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (Objects.nonNull(repository) && RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            Dict existsDict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.HANDLER_MAVEN_INDEXER.getType()).build());
            String comment = "迁移中";
            if (Objects.nonNull(existsDict) && comment.equals(existsDict.getComment())) {
                return;
            }

            RemoteRepository remoteRepository = repository.getRemoteRepository();
            String remoteRepositoryUrl = remoteRepository.getUrl(), indexPath = ".index/nexus-maven-repository-index.properties";
            remoteRepositoryUrl = StringUtils.removeEnd(remoteRepositoryUrl, "/");
            String indexUrl = String.format("%s/%s", remoteRepositoryUrl, indexPath);
            Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            Response response = null;
            try {
                WebTarget service = restClient.target(indexUrl);
                authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
                log.info("Get maven index properties {} start", indexUrl);
                response = service.request().get();
                if (response.getStatus() != HttpStatus.SC_OK) {
                    log.warn("Get maven index properties {} error {}", indexUrl, response.getStatus());
                    return;
                }
                String indexProperties = response.readEntity(String.class), id, chainId, timestamp;
                id = extractValue(indexProperties, "nexus.index.id=(.*?)\\n");
                chainId = extractValue(indexProperties, "nexus.index.chain-id=(.*?)\\n");
                timestamp = extractValue(indexProperties, "nexus.index.timestamp=(.*?)\\n");
                if (StringUtils.isBlank(id) || StringUtils.isBlank(chainId)) {
                    log.warn("Get maven index properties {} id {} chainId {} timestamp {} has null value", indexProperties, id, chainId, timestamp);
                    return;
                }
                log.info("Get maven index properties {} id {} chainId {} timestamp {}", indexProperties, id, chainId, timestamp);
                String mavenIndexerPath = mavenIndexerService.storeMavenIndexer("json", id, chainId, remoteRepositoryUrl);
                if (StringUtils.isNotBlank(mavenIndexerPath)) {
                    mavenIndexerService.handlerMavenIndexerAndDownLoad(userDetails.getUsername(), repository, mavenIndexerPath, syncArtifactForm.getBatch(), null);
                }
            } catch (Exception e) {
                log.error("Failed to download {} error {}", indexUrl, e);
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
                restClient.close();
            }
        }
    }

    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {
        // 获取仓库信息
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
            if (syncArtifactForm.getSyncer() != null) {
                syncArtifactForm.getSyncer().close();
            }

        }
    }

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
        if (customAuthenticationFeature != null) {
            webTarget.register(customAuthenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }

    /**
     * 提取信息
     *
     * @param input   原字符串
     * @param pattern 正则表达式
     * @return 结果
     */
    private static String extractValue(String input, String pattern) {
        Pattern regexPattern = Pattern.compile(pattern);
        Matcher matcher = regexPattern.matcher(input);
        if (matcher.find()) {
            return matcher.group(1);
        } else {
            return null;
        }
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
     * @param repository  repository
     * @param rootUrl     rootUrl
     * @param url         当前url
     * @param sleepMillis 睡眠毫秒数
     * @param dom         页面元素
     * @param file        文件
     * @param writer      writer
     */
    private boolean findSubUrl(Repository repository, String rootUrl, String url, Integer sleepMillis, String dom, File file, BufferedWriter writer) {
        try {
            if (isSuffix(url)) {
                return true;
            }
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            return artifactComponent.parseLinksStreaming(repository, url, absUrl -> {
                if (isSuffix(absUrl)) {
                    absUrl = StringUtils.removeStart(absUrl.replace(rootUrl, ""), GlobalConstants.SEPARATOR);
                    filesCommonComponent.storeContent(absUrl, file.getParent() + "/artifact");
                    THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
                    distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + repository.getStorageIdAndRepositoryId()).addAndGet(1);
                } else {
                    // 非子目录
                    if (!absUrl.contains(url) || url.equals(absUrl) || !absUrl.endsWith("/")) {
                        return;
                    }
                    String path = absUrl.substring(rootUrl.length());
                    try {
                        writer.write(path + "\n");
                        distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_TOTAl + repository.getStorageIdAndRepositoryId()).addAndGet(1L);
                    } catch (IOException e) {
                        log.error("路径{}写异常", path);
                    }
                }
            });
//            Document doc = artifactComponent.getDocument(repository, url);
//            if (Objects.isNull(doc)) {
//                log.error("获取文件失败");
//                return true;
//            }
//            Elements links = doc.select(dom);
//            for (Element link : links) {
//                String absUrl = link.absUrl("href");
//                if (isSuffix(absUrl)) {
//                    absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
//                    filesCommonComponent.storeContent(absUrl, file.getParent() + "/artifact");
//                    THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
//                    distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + repository.getStorageIdAndRepositoryId()).addAndGet(1);
//                } else {
//                    // 非子目录
//                    if (!absUrl.contains(url) || url.equals(absUrl) || !absUrl.endsWith("/")) {
//                        continue;
//                    }
//                    String path = absUrl.substring(rootUrl.length());
//                    writer.write(path + "\n");
//                }
//            }
        } catch (Exception e) {
            log.error("Maven包索引，错误 [{}]", ExceptionUtils.getStackTrace(e));
            return false;
        }

    }

    private String syncPackageIndex(SyncArtifactForm syncArtifactForm) {
        try {
            long startTime = System.currentTimeMillis();
            distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + syncArtifactForm.getStoreAndRepo()).set(0);
            distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_TOTAl + syncArtifactForm.getStoreAndRepo()).set(0L);
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
            if (syncArtifactForm.getSyncMeta() == 1 && syncArtifactForm.getSyncer() == null) {
                String apiUrl = remoteUrl.substring(0, remoteUrl.indexOf(repository.getId()));
                JfrogPropertySyncer syncer = new JfrogPropertySyncer(apiUrl, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
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
            log.info("Maven包索引同步，仓库地址 [{}] 存放爬取信息的目录 [{}]", repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("Maven包索引同步存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", dirPath, flag);
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
                log.info("Maven包索引同步存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", rootFile.getAbsolutePath(), flag);
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
                try (Scanner scanner = new Scanner(urlFile); BufferedWriter writer = new BufferedWriter(new FileWriter(subFile))) {
                    while (scanner.hasNext()) {
                        String line = scanner.nextLine();
                        if (StringUtils.isNotBlank(line)) {
                            if (!line.startsWith(separator)) {
                                line = separator + line;
                            }
                            fileEmpty = false;
                            String url = rootUrl + line;
                            if (!findSubUrl(repository, rootUrl, url, sleepMillis, syncArtifactForm.getDom(), subFile, writer)) {
                            }
                        }
                    }
                } catch (IOException e) {
                    log.error("Maven包索引同步错误 [{}]", ExceptionUtils.getStackTrace(e));
                    return null;
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            log.info("Maven包索引同步完成耗时 [{}] ms, 同步制品总个数 [{}]", System.currentTimeMillis() - startTime, THREAD_LOCAL.get());
            syncArtifactForm.setTotalArtifact(THREAD_LOCAL.get());
            return dirPath;
        } catch (Exception e) {
            log.error("Maven包索引同步，错误 [{}]", ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL.remove();
        }
        return null;
    }

    /**
     * 判断后缀
     *
     * @param url url
     * @return true 后缀匹配 false 后缀不匹配
     */
    private boolean isSuffix(String url) {
        return suffixList.stream().anyMatch(url::endsWith);
    }

    private boolean handlerPath(String dirPath, SyncArtifactForm syncArtifactForm) {
        long allStartTime = System.currentTimeMillis();
        Path path = Path.of(dirPath + "/artifact");
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return syncArtifactForm.getTotalArtifact() == 0;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        int availableCores = syncArtifactForm.getMaxThreadNum() == null ? commonComponent.getAvailableCores() * 2 : syncArtifactForm.getMaxThreadNum();
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browseMavenSync", availableCores, availableCores);
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
        handlerDirectoryMetadata(dirPath, syncArtifactForm);
        int total = (int) distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + syncArtifactForm.getStoreAndRepo()).get();
        syncArtifactForm.setSyncMount(total);
        log.info("Maven包同步完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个制品，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), total, System.currentTimeMillis() - allStartTime);
        return !ispaused;
    }

    private boolean handlerDirectoryMetadata(String dirPath, SyncArtifactForm syncArtifactForm) {
        long allStartTime = System.currentTimeMillis();
        Path path = Path.of(dirPath);
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return syncArtifactForm.getTotalArtifact() == 0;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_COUNT + syncArtifactForm.getStoreAndRepo()).set(0L);
        int availableCores = commonComponent.getAvailableCores() * 2;
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browseMavenSync", availableCores, availableCores);
        boolean ispaused = false;
        String levelPrefix = "level_";
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
            ispaused = pathStream.filter(item -> Files.isRegularFile(item) && item.getFileName().toString().startsWith(levelPrefix)).anyMatch(item -> {
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
                                flag = false;
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm, pathList, threadPoolTaskExecutor);
                        }
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
        int directoryTotal = (int) distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_TOTAl + syncArtifactForm.getStoreAndRepo()).get();
        log.info("Maven包同步目录元数据完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个目录元数据，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), directoryTotal, System.currentTimeMillis() - allStartTime);
        return !ispaused;
    }

    private void batchDownload(Path path, SyncArtifactForm form, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        String storageId = form.getStorageId();
        String repositoryId = form.getRepositoryId();
        List<List<String>> artifactPathLists = Lists.partition(artifactPathList, 5);
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        String levelPrefix = "level_", fileName = path.getFileName().toString();
        for (List<String> itemArtifactPathList : artifactPathLists) {
            futureTask = new FutureTask<String>(() -> {
                for (String artifactPath : itemArtifactPathList) {
                    try {
                        if (StringUtils.isNotBlank(artifactPath)) {
                            //制品
                            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                            if (fileName.startsWith(levelPrefix) && (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath))) {
                                //是目录的索引文件，并且在同步目录元数据时，该目录不存在，跳过处理
                                continue;
                            }
                            if (Files.exists(repositoryPath) && Files.isDirectory(repositoryPath)) {
                                //目录
                                JfrogPropertySyncer syncer = form.getSyncer();
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_COUNT + form.getStoreAndRepo()).addAndGet(1L);
                                if (syncer != null) {
                                    String properties = syncer.getPropertiesByKeyAndPath(repositoryId, artifactPath);
                                    if (properties != null) {
                                        artifactWebService.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
                                    }
                                }
                                continue;
                            }
                            if (Files.exists(repositoryPath)) {
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + storageId + ":" + repositoryId).addAndGet(1L);
                                log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                                continue;
                            }
                            artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                // 添加成功 计数
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + storageId + ":" + repositoryId).addAndGet(1L);
                                // 同步元数据
                                JfrogPropertySyncer syncer = form.getSyncer();
                                if (syncer != null) {
                                    String properties = syncer.getPropertiesByKeyAndPath(repositoryId, artifactPath);
                                    if (properties != null) {
                                        artifactWebService.saveArtifactMetaByString(storageId, repositoryId, artifactPath, properties);
                                    }
                                }
                            }
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


}
