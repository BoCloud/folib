package com.veadan.folib.components.syncartifact;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.migrate.SyncArtifactForm;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.util.StopWatch;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Objects;
import java.util.Scanner;

/**
 * @author huayanjun
 * @since 2025-01-20 14:54
 */
@Slf4j
public abstract class BaseArtifactProvider implements SyncArtifactProvider {

    private final String tempPath;
    private final SyncUtils syncUtils;
    private final ArtifactComponent artifactComponent;


    public BaseArtifactProvider(String tempPath, SyncUtils syncUtils, ArtifactComponent artifactComponent) {
        this.tempPath = tempPath;
        this.syncUtils = syncUtils;
        this.artifactComponent = artifactComponent;
    }

    @Override
    public abstract void register();

    public abstract String getLayout();

    @Override
    public void browseFullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void fullSync(SyncArtifactForm syncArtifactForm) {

    }

    @Override
    public void batchBrowseSync(SyncArtifactForm syncArtifactForm) {

    }

    public abstract boolean isArtifact(String url);


    /**
     * @param currentUrl 当前获取的url
     * @param preUrl     上一级url
     * @return 为上一级的子目录则为true
     */
    public boolean isSubDirectory(String currentUrl, String preUrl) {
        return currentUrl.contains(preUrl) && !currentUrl.equals(preUrl);
    }


    protected boolean findSubUrl(Repository repository, String rootUrl, String url, String remoteUrl, Integer sleepMillis, File file, FileWriter writer) {
        try {
            if (Objects.nonNull(sleepMillis)) {
                Thread.sleep(sleepMillis);
            }
            return artifactComponent.parseLinksStreaming(repository, url, absUrl -> {
                try {
                    absUrl = UriUtils.decode(absUrl);
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                    return;
                }
                if (isArtifact(absUrl)) {
                    absUrl = StringUtils.removeStart(absUrl.replace(remoteUrl, ""), GlobalConstants.SEPARATOR);
                    syncUtils.storeContent(absUrl, file.getParent() + "/artifact");
                    syncUtils.indexIncrease(repository.getStorageIdAndRepositoryId());
                }
                if (isSubDirectory(absUrl, url)) {
                    String path = absUrl.substring(rootUrl.length());
                    try {
                        writer.write(path + "\n");
                        writer.flush();
                    } catch (IOException e) {
                        log.error("写索引文件{}异常{}", absUrl, e.getMessage(), e);
                    }
                }
            });
        } catch (Exception e) {
            log.error("【{}】包索引同步制品，错误 [{}]", getLayout(), ExceptionUtils.getStackTrace(e));
            return false;
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
            if (baseUri.endsWith(File.separator)) {
                baseUri = baseUri.substring(0, baseUri.lastIndexOf(File.separator));
            }
            String repositoryBaseUri = String.format("%s/storages/%s/%s", baseUri, syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            String remoteUrl = repository.getRemoteRepository().getUrl();
            if (remoteUrl.endsWith(File.separator)) {
                remoteUrl = remoteUrl.substring(0, remoteUrl.lastIndexOf(File.separator));
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
            if (rootUrl.endsWith(File.separator)) {
                rootUrl = rootUrl.substring(0, rootUrl.lastIndexOf(File.separator));
            }
            String dirPath = tempPath + File.separator + "syncArtifact" + File.separator + syncArtifactForm.getStorageId() + File.separator + syncArtifactForm.getRepositoryId();
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
                            if (!line.startsWith(File.separator)) {
                                line = File.separator + line;
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
            int total = syncUtils.getArtifactCount(syncArtifactForm.getStoreAndRepo());
            log.info("【{}】包索引同步完成耗时 【{}】 s, 同步制品总个数 [{}]", getLayout(), sw.getTotalTimeSeconds(), total);
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


}
