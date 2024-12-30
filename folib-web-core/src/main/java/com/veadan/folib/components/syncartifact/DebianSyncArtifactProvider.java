package com.veadan.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DistributedCounterComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.components.files.FilesCommonComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.ArtifactSyncTypeEnum;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.MigrateStatusEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.JfrogMigrateService;
import com.veadan.folib.services.NpmService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class DebianSyncArtifactProvider implements SyncArtifactProvider {

    @Value("${folib.temp}")
    private String tempPath;

    private static final ThreadLocal<Integer> THREAD_LOCAL = ThreadLocal.withInitial(() -> 0);

    /**
     * 计数
     */
    private static final AtomicLong COUNT = new AtomicLong(0);

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


    @Resource
    private ClusterSyncService clusterSyncService;

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private DistributedCacheComponent distributedCacheComponent;
    @Resource
    private ConfigurationManagementService configurationManagementService;

    private static final Pattern PACKAGE_REGX = Pattern.compile(".*/dists/([^/]+)/([^/]+)/binary-([^/]+)/");


    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(ArtifactSyncTypeEnum.DEBIAN.getType(), this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactSyncTypeEnum.DEBIAN.getType());
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

    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {
        // 获取仓库信息
        String storageId = syncArtifactForm.getStorageId(), repositoryId = syncArtifactForm.getRepositoryId();
        MutableConfiguration mutableConfigurationClone = configurationManagementService.getMutableConfigurationClone();
        RepositoryDto repository = mutableConfigurationClone.getStorage(storageId).getRepository(repositoryId);
        if(MigrateStatusEnum.QUEUING.getStatus()==repository.getSyncStatus()||MigrateStatusEnum.INDEX_FAILED.getStatus()==repository.getSyncStatus()){
            updateAndSyncRepoStatus(syncArtifactForm.getStoreAndRepo(),MigrateStatusEnum.FETCHING_INDEX.getStatus());
            String dirPath = syncPackageIndex(syncArtifactForm);
            if(dirPath==null){
                log.info("同步索引失败,请稍后重试");
                updateAndSyncRepoStatus(syncArtifactForm.getStoreAndRepo(),MigrateStatusEnum.INDEX_FAILED.getStatus());
                return;
            }
            repository.setSyncDirPath(dirPath);
            repository.setTotalArtifact(syncArtifactForm.getTotalArtifact());
            repository.setSyncStatus(MigrateStatusEnum.SYNCING_ARTIFACT.getStatus());
            // 更新状态
            try {
                configurationManagementService.saveRepository(storageId,repository);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
            } catch (IOException e) {
                log.info("更新数据失败");
            }
        }
        String path=repository.getSyncDirPath();
        distributedCacheComponent.put(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo(),"1");
        if (handlerPath(path, syncArtifactForm)) {
            updateAndSyncRepoStatus(syncArtifactForm.getStoreAndRepo(),MigrateStatusEnum.COMPLETED.getStatus());
        }else {
            updateAndSyncRepoStatus(syncArtifactForm.getStoreAndRepo(),MigrateStatusEnum.PAUSED.getStatus());
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
        final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? HttpAuthenticationFeature.basic(username, password) : null;
        if (authenticationFeature != null) {
            webTarget.register(authenticationFeature);
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
     * @param file        文件
     * @param writer      writer
     */
    private boolean findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, String dom, File file, FileWriter writer) {
        try {
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            Document doc = artifactComponent.getDocument(repository, url);
            if(Objects.isNull(doc)){
                log.error("获取文件失败");
                return false;
            }
            Elements links = doc.select(dom);
            for (Element link : links) {
                String absUrl = link.absUrl("href");
                if(absUrl.endsWith("Release")||absUrl.endsWith("Packages.bz2")||absUrl.endsWith("Packages.gz")){
                    absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(),absUrl);
                    if(!Files.exists(repositoryPath)){
                        artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), absUrl);
                    }
                }
                else if (absUrl.endsWith("Packages")) {
                    // 下载这个文件
                    Matcher match = PACKAGE_REGX.matcher(url);
                    if(match.matches()){
                        String distribution = match.group(1);
                        String component = match.group(2);
                        String architecture = match.group(3);
                        String path =file.getParent()+"/"+distribution+"_"+component+"_"+architecture+"/Packages";
                        String dist=file.getParent()+"/"+"/artifact";
                        artifactComponent.getArtifactByUrl(repository,absUrl,path);
                        parsePackagesFile(path,distribution,component,architecture,dist);
                        absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(),absUrl);
                        if(!Files.exists(repositoryPath)){
                            artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), absUrl);
                        }
                    }
                } else {
                    // 非子目录
                    if (!absUrl.contains(url) || url.equals(absUrl)) {
                        continue;
                    }
                    if(!absUrl.endsWith(GlobalConstants.SEPARATOR)){
                        return true;
                    }
                    String path = absUrl.substring(rootUrl.length());
                    writer.write(path + "\n");
                    writer.flush();
                }
            }
        } catch (Exception e) {
            log.error("debian包索引，错误 [{}]", ExceptionUtils.getStackTrace(e));
            return false;
        }
        return true;
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
            log.info("debian包索引同步，仓库地址 [{}] 存放爬取信息的目录 [{}]", repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("debian包索引同步存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", dirPath, flag);
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
                log.info("debian包索引同步存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", rootFile.getAbsolutePath(), flag);
                try (FileWriter writer = new FileWriter(rootFile)) {
                    writer.write("/\n");
                    writer.flush();
                }
            }
            File urlFile;
            while ((urlFile = getLevelFile(dir, level)).exists()) {
                level++;
                log.info("开始检索{}下的目录",urlFile);
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
                            if (!findSubUrl(repository, rootUrl, url, remoteUrl, sleepMillis, syncArtifactForm.getDom(), subFile, writer)) {
                                return null;
                            }
                        }
                    }
                } catch (IOException e) {
                    log.error("debian包索引同步错误 [{}]", ExceptionUtils.getStackTrace(e));
                    return null;
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            log.info("debian包索引同步完成耗时 [{}] ms, 同步制品总个数 [{}]", System.currentTimeMillis() - startTime, THREAD_LOCAL.get());
            syncArtifactForm.setTotalArtifact(THREAD_LOCAL.get());
            return dirPath;
        } catch (Exception e) {
            log.error("debian包索引同步，错误 [{}]", ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL.remove();
        }
        return null;
    }

    /**
     * 判断后缀
     * @return true 后缀匹配 false 后缀不匹配
     */
    private boolean handlerPath(String dirPath, SyncArtifactForm syncArtifactForm) {
        long allStartTime = System.currentTimeMillis();
        Path path = Path.of(dirPath + "/artifact");
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return false;
        }
        int batch = 100;
        if (Objects.nonNull(syncArtifactForm.getBatch())) {
            batch = syncArtifactForm.getBatch();
        }
        COUNT.set(0L);
        int availableCores = commonComponent.getAvailableCores() * 2;
        ThreadPoolTaskExecutor threadPoolTaskExecutor = commonComponent.buildThreadPoolTaskExecutor("browsedebianSync", availableCores, availableCores);
        boolean isPaused=false;
        try (Stream<Path> pathStream = Files.list(path)) {
            int finalBatch = batch;
             isPaused= pathStream.sorted().anyMatch(item -> {
                String currentLine = "";
                long lines = 0, startTime = System.currentTimeMillis();
                boolean flag=true;
                try {
                    List<String> pathList = Lists.newArrayList();
                    try (LineIterator lineIterator = FileUtils.lineIterator(item.toFile(), "UTF-8")) {
                        while (lineIterator.hasNext()) {
                            if("0".equals(distributedCacheComponent.get(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo()))){
                                log.info("仓库{}同步任务暂停",syncArtifactForm.getStoreAndRepo());
                                updateAndSyncRepoStatus(syncArtifactForm.getStoreAndRepo(),MigrateStatusEnum.PAUSED.getStatus());
                                distributedCacheComponent.delete(JfrogMigrateService.PAUSED_FLAG_PRE+syncArtifactForm.getStoreAndRepo());
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
                                    batchDownload(item, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), pathList, threadPoolTaskExecutor);
                                }
                            } catch (Exception ex) {
                                log.error(ExceptionUtils.getStackTrace(ex));
                                flag=false;
                            }
                        }
                        if (CollectionUtils.isNotEmpty(pathList)) {
                            batchDownload(item, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), pathList, threadPoolTaskExecutor);
                        }
                    }
                    // 清除已完成的文件
                    if(flag){
                        Files.delete(item);
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
        log.info("debian包同步完成，存储空间 [{}] 仓库 [{}] 同步 [{}] 个制品，耗时 [{}] ms", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId(), COUNT.get(), System.currentTimeMillis() - allStartTime);
        return !isPaused;
    }

    private void batchDownload(Path path, String storageId, String repositoryId, List<String> artifactPathList, ThreadPoolTaskExecutor threadPoolTaskExecutor) {
        if (CollectionUtils.isEmpty(artifactPathList)) {
            return;
        }
        List<List<String>> artifactPathLists = Lists.partition(artifactPathList, 5);
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask;
        for (List<String> itemArtifactPathList : artifactPathLists) {
            futureTask = new FutureTask<>(() -> {
                for (String artifactPath : itemArtifactPathList) {
                    try {
                        if (StringUtils.isNotBlank(artifactPath)) {
                            //制品
                            String[] paths= artifactPath.split(":");
                            String distribution=paths[0];
                            String component=paths[1];
                            String architecture=paths[2];
                            artifactPath=paths[3]+";"+DebianUtils.getArrtString(distribution,component,architecture);
                            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                log.debug("Batch download storageId [{}] repositoryId [{}] artifactPath [{}] exists skip..", storageId, repositoryId, artifactPath);
                                continue;
                            }
                            artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
                            if (Files.exists(repositoryPath)) {
                                COUNT.incrementAndGet();
                                // 添加成功 计数
                                distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT+storageId+":"+repositoryId).addAndGet(1L);
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

    private void updateAndSyncRepoStatus(String storeAndRepo, int status) {
        String storageId = ConfigurationUtils.getStorageId(storeAndRepo,storeAndRepo);
        String repositoryId=ConfigurationUtils.getRepositoryId(storeAndRepo);
        MutableConfiguration mutableConfigurationClone = configurationManagementService.getMutableConfigurationClone();
        RepositoryDto repository = mutableConfigurationClone.getStorage(storageId).getRepository(repositoryId);
        repository.setSyncStatus(status);
        try {
            configurationManagementService.saveRepository(storageId,repository);
            SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, SyncRepositoryEnum.ADD_OR_UPDATE);
            clusterSyncService.syncRepository(syncRepositoryDto);
        } catch (IOException e) {
            log.info("更新数据失败");
        }
    }

    public void parsePackagesFile(String filePath, String distribution, String component, String architecture,String distPath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            String filename = null;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("Filename: ")) {
                    filename = line.substring(10).trim();
                }
                // 如果找到完整的 Filename，记录信息
                if (filename != null) {
                    String str=distribution + ":" + component + ":" + architecture + ":" + filename;
                    filesCommonComponent.storeContent(str, distPath);
                    THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
                    filename = null; // 重置
                }
            }

        }
    }
}
