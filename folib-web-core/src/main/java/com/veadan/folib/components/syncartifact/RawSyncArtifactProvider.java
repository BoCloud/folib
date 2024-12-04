package com.veadan.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.components.files.FilesCommonComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
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
 * @author leipenghui
 **/
@Slf4j
@Component
public class RawSyncArtifactProvider implements SyncArtifactProvider {

    /**
     * 计数
     */
    private static final ThreadLocal<Integer> THREAD_LOCAL = ThreadLocal.withInitial(() -> 0);

    /**
     * 计数
     */
    private static final AtomicLong COUNT = new AtomicLong(0);

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private RawLayoutProvider rawLayoutProvider;

    @Inject
    private FilesCommonComponent filesCommonComponent;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.RAW.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.RAW.getType());
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
     * @param remoteUrl   remoteUrl
     * @param sleepMillis 睡眠毫秒数
     * @param dom         页面元素
     * @param file        file
     * @param writer      writer
     */
    private void findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, String dom, File file, FileWriter writer) {
        try {
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            String separator = "/";
            Document doc = artifactComponent.getDocument(repository, url);
            Elements links = doc.select(dom);
            for (Element link : links) {
                String absUrl = link.absUrl("href");
                if (rawLayoutProvider.isChecksum(absUrl)) {
                    continue;
                }
                if (!absUrl.endsWith(separator)) {
                    absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
                    filesCommonComponent.storeContent(absUrl, file.getParent() + "/artifact");
                    THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
                } else {
                    // 非子目录
                    if (!absUrl.contains(url) || url.equals(absUrl)) {
                        continue;
                    }
                    String path = absUrl.substring(rootUrl.length());
                    writer.write(path + "\n");
                    writer.flush();
                }
            }
        } catch (Exception e) {
            log.error("Raw包索引同步制品，错误 [{}]", ExceptionUtils.getStackTrace(e));
        }
    }

    private String syncPackageIndex(SyncArtifactForm syncArtifactForm) {
        try {
            long startTime = System.currentTimeMillis();
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
            String rootUrl = remoteUrl;
            if (StringUtils.isNotBlank(syncArtifactForm.getBrowseUrl())) {
                rootUrl = syncArtifactForm.getBrowseUrl();
            }
            if (rootUrl.endsWith(separator)) {
                rootUrl = rootUrl.substring(0, rootUrl.lastIndexOf(separator));
            }
            String dirPath = tempPath + File.separator + "syncArtifact" + File.separator + syncArtifactForm.getStorageId() + File.separator + syncArtifactForm.getRepositoryId();
            log.info("Raw包索引同步， 仓库地址 [{}] 存放爬取信息的目录 [{}]", repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("Raw包索引同步存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", dirPath, flag);
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
                log.info("Raw包索引同步存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", rootFile.getAbsolutePath(), flag);
                try (FileWriter writer = new FileWriter(rootFile)) {
                    writer.write("/\n");
                    writer.flush();
                }
            }
            File urlFile;
            while ((urlFile = getLevelFile(dir, level)).exists()) {
                level++;
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
                            findSubUrl(repository, rootUrl, url, remoteUrl, sleepMillis, syncArtifactForm.getDom(), subFile, writer);
                        }
                    }
                } catch (IOException e) {
                    log.error("Raw包索引同步错误 [{}]", ExceptionUtils.getStackTrace(e));
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            log.info("Raw包索引同步完成耗时 [{}] ms, 同步制品总个数 [{}]", System.currentTimeMillis() - startTime, THREAD_LOCAL.get());
            return dirPath;
        } catch (Exception e) {
            log.error("Raw包索引同步，错误 [{}]", ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL.remove();
        }
        return "";
    }

    private void handlerPath(String dirPath, SyncArtifactForm syncArtifactForm) {
        long allStartTime = System.currentTimeMillis();
        Path path = Path.of(dirPath + "/artifact");
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        COUNT.set(0L);
        int availableCores = commonComponent.getAvailableCores() * 2;
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browseRawSync", availableCores, availableCores);
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
            pathStream.forEach(item -> {
                String currentLine = "";
                long lines = 0, startTime = System.currentTimeMillis();
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
                                    batchDownload(item, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), pathList, threadPoolTaskExecutor);
                                }
                            } catch (Exception ex) {
                                log.error(ExceptionUtils.getStackTrace(ex));
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), pathList, threadPoolTaskExecutor);
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] lines [{}] error [{}] ms", item.toString(), lines, ExceptionUtils.getStackTrace(ex));
                }
                log.info("Handle path [{}] lines [{}] finished take time [{}] ms", item.toString(), lines, System.currentTimeMillis() - startTime);
            });
        } catch (Exception ex) {
            log.error("Error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        log.info("Raw包同步完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个制品，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), COUNT.get(), System.currentTimeMillis() - allStartTime);
    }

    private void batchDownload(Path path, String storageId, String repositoryId, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        List<List<String>> artifactPathLists = Lists.partition(artifactPathList, 5);
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (List<String> itemArtifactPathList : artifactPathLists) {
            futureTask = new FutureTask<String>(() -> {
                for (String artifactPath : itemArtifactPathList) {
                    try {
                        if (StringUtils.isNotBlank(artifactPath)) {
                            //制品
                            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                COUNT.incrementAndGet();
                                log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                                continue;
                            }
                            artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                COUNT.incrementAndGet();
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
