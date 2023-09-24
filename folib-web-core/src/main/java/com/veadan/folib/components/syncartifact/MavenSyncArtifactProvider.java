package com.veadan.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import cn.hutool.http.HttpUtil;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.MavenIndexerService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
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
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author leipenghui
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

    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(Maven2LayoutProvider.ALIAS, this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), Maven2LayoutProvider.ALIAS);
    }

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {
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
            log.info("[{}] repositoryBaseUri [{}] maven存放爬取信息的目录 [{}]", this.getClass().getSimpleName(), repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("[{}] maven存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", this.getClass().getSimpleName(), dirPath, flag);
            }
            Integer sleepMillis = null;
            if (Objects.nonNull(syncArtifactForm.getSleepMillis())) {
                sleepMillis = syncArtifactForm.getSleepMillis();
            }
            int level = 0;
            File rootFile = getLevelFile(dir, level);
            if (!rootFile.exists()) {
                boolean flag = rootFile.createNewFile();
                log.info("[{}] maven存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", this.getClass().getSimpleName(), rootFile.getAbsolutePath(), flag);
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
                            findSubUrl(repository, rootUrl, url, remoteUrl, sleepMillis, syncArtifactForm.getDom(), writer, repositoryBaseUri);
                        }
                    }
                } catch (IOException e) {
                    log.error("[{}] maven全量同步制品错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            log.info("[{}] maven全量同步制品耗时 [{}] ms, 同步制品总个数 [{}]", this.getClass().getSimpleName(), System.currentTimeMillis() - startTime, THREAD_LOCAL.get());
        } catch (Exception e) {
            log.error("[{}] maven全量同步制品，fullSync错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL.remove();
        }
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
     * @param repository        repository
     * @param rootUrl           rootUrl
     * @param url               当前url
     * @param remoteUrl         remoteUrl
     * @param sleepMillis       睡眠毫秒数
     * @param dom               页面元素
     * @param writer            writer
     * @param repositoryBaseUri repositoryBaseUri
     */
    private void findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, String dom, FileWriter writer, String repositoryBaseUri) {
        try {
            log.info("[{}] maven全量同步 findSubUrl url [{}]", this.getClass().getSimpleName(), url);
            if (isSuffix(url)) {
                return;
            }
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            Document doc = artifactComponent.getDocument(repository, url);
            Elements links = doc.select(dom);
            log.info("[{}] maven全量同步 findSubUrl links [{}]", this.getClass().getSimpleName(), links.toString());
            for (Element link : links) {
                String absUrl = link.absUrl("href");
                if (isSuffix(absUrl)) {
                    absUrl = absUrl.replace(remoteUrl, repositoryBaseUri);
                    log.info("[{}] maven全量同步 findSubUrl absUrl [{}]", this.getClass().getSimpleName(), absUrl);
                    HttpUtil.get(absUrl);
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
            log.error("[{}] maven全量同步制品，findSubUrl错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
        }
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
}
