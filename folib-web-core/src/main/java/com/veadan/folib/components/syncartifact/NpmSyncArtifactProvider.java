package com.veadan.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import cn.hutool.http.HttpUtil;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.forms.syncartifact.SyncArtifactForm;
import com.veadan.folib.providers.layout.NpmLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class NpmSyncArtifactProvider implements SyncArtifactProvider {

    /**
     * 计数
     */
    private static final ThreadLocal<Integer> THREAD_LOCAL = ThreadLocal.withInitial(() -> 0);

    /**
     * 匹配后缀
     */
    private final List<String> suffixList = new ArrayList<>(Arrays.asList(".tgz".split(",")));

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @PostConstruct
    @Override
    public void register() {
        syncArtifactProviderRegistry.addProvider(NpmLayoutProvider.ALIAS, this);
        log.info("Registered sync artifact '{}' with alias '{}'.",
                getClass().getCanonicalName(), NpmLayoutProvider.ALIAS);
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
            log.info("[{}] repositoryBaseUri [{}] npm存放爬取信息的目录 [{}]", this.getClass().getSimpleName(), repositoryBaseUri, dirPath);
            File dir = new File(dirPath);
            if (!dir.exists()) {
                boolean flag = dir.mkdirs();
                log.info("[{}] npm存放爬取信息的目录 [{}] 不存在，创建状态 [{}]", this.getClass().getSimpleName(), dirPath, flag);
            }
            Integer sleepMillis = null;
            if (Objects.nonNull(syncArtifactForm.getSleepMillis())) {
                sleepMillis = syncArtifactForm.getSleepMillis();
            }
            int level = 0;
            File rootFile = getLevelFile(dir, level);
            if (!rootFile.exists()) {
                boolean flag = rootFile.createNewFile();
                log.info("[{}] npm存放爬取信息的文件 [{}] 不存在，创建状态 [{}]", this.getClass().getSimpleName(), rootFile.getAbsolutePath(), flag);
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
                    log.error("[{}] npm全量同步制品错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
                }
                if (fileEmpty) {
                    FileUtil.del(urlFile);
                    FileUtil.del(subFile);
                    break;
                }
            }
            log.info("[{}] npm全量同步制品耗时 [{}] ms, 同步制品总个数 [{}]", this.getClass().getSimpleName(), System.currentTimeMillis() - startTime, THREAD_LOCAL.get());
        } catch (Exception e) {
            log.error("[{}] npm全量同步制品，fullSync错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
        } finally {
            THREAD_LOCAL.remove();
        }
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
            log.info("[{}] npm全量同步 findSubUrl url [{}]", this.getClass().getSimpleName(), url);
            if (isSuffix(url)) {
                return;
            }
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            String separator = "/";
            Document doc = artifactComponent.getDocument(repository, url);
            Elements links = doc.select(dom);
            log.info("[{}] npm全量同步 findSubUrl links [{}]", this.getClass().getSimpleName(), links.toString());
            for (Element link : links) {
                String absUrl = link.absUrl("href");
                if (isSuffix(absUrl)) {
                    absUrl = absUrl.replace(remoteUrl, repositoryBaseUri);
                    log.info("[{}] npm全量同步 findSubUrl absUrl [{}]", this.getClass().getSimpleName(), absUrl);
                    HttpUtil.get(absUrl);
                    THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
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
                        absUrl = absUrl.replace(separator, "");
                        absUrl = repositoryBaseUri + File.separator + absUrl;
                        log.info("[{}] npm全量同步 findSubUrl versions absUrl [{}]", this.getClass().getSimpleName(), absUrl);
                        HttpUtil.get(absUrl);
                        THREAD_LOCAL.set(THREAD_LOCAL.get() + 1);
                    }
                }
            }
        } catch (Exception e) {
            log.error("[{}] npm全量同步制品，findSubUrl错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(e));
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
