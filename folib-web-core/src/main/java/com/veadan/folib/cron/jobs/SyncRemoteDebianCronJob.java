package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobAliasNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobBooleanTypeField;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.Resource;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

/**
 * @author huayanjun
 * @since 2025-02-06 17:14
 */
@Slf4j
public class SyncRemoteDebianCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String BACKUP_INCREASE = "backupIncrease";

    // 需要下载的发型版/组件/架构等信息 以逗号格开，如果为null和空则下载全部
    private static final String SCOPE = "debianScopeKey";
    public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");
    @Value("${folib.temp}")
    private String tempPath;

    @Resource
    private ConfigurationManager configurationManager;

    @Resource
    private DictService dictService;

    @Resource
    private ArtifactComponent artifactComponent;

    @Resource
    private ArtifactResolutionService artifactResolutionService;

    @Resource
    private ReplicationBackup replicationBackup;

    private final String DICT_TYPE = "repository_replication_task";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobBooleanTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(BACKUP_INCREASE), "是否备份增量信息"))));

    @Override
    protected void executeTask(CronTaskConfigurationDto config) {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String scope = config.getProperty(SCOPE);
        Storage storage = configurationManager.getStorage(storageId);
        if (Objects.isNull(storage)) {
            log.warn("Storage [{}] not found", storageId);
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            log.warn("Storage [{}] repository [{}] not found", storageId, repositoryId);
            return;
        }
        if (!RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            log.warn("Repository storageId [{}] repositoryId [{}] not is proxy type skip..", storageId, repositoryId);
            return;
        }
        // 获取dict数据查看是否有相关数据 1.没有全量同步 2.有增量同步
        Dict query = new Dict().setDictType(DICT_TYPE).setDictKey(repository.getStorageIdAndRepositoryId());
        Dict lasted = dictService.selectLatestOneDict(query);
        // 没有同步过 开始全量同步
        boolean backup = Boolean.parseBoolean(config.getProperty(BACKUP_INCREASE)) && Objects.nonNull(lasted);
        replication(repository, scope, lasted, backup);
    }

    void replication(Repository repository, String scope, Dict dict, boolean backup) {
        Date date = new Date();
        String dateStr = DATE_FORMAT.format(date);
        String preDate = dict == null ? null : DATE_FORMAT.format(dict.getCreateTime());
        String tempDir = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId();
        Set<String> updates = backup ? null : new HashSet<>();
        if (scope == null) {
            String remoteUrl = StringUtils.removeEnd(repository.getRemoteRepository().getUrl(), "/") + "/dists/";
            syncAllPackagesGz(repository, remoteUrl, Path.of(tempDir), preDate, dateStr, updates);
        } else {
            String[] groups = scope.split(",");
            for (String group : groups) {
                String[] comp = group.split(":");
                if (comp.length != 3) {
                    log.error("无效的组件格式");
                    continue;
                }
                String packagesGzPath = String.format("dists/%s/%s/binary-%s/Packages.gz", comp[0], comp[1], comp[2]);
                syncSpecificPackagesGz(repository, packagesGzPath, tempDir, dateStr, preDate, updates);
            }
        }
        Dict current = new Dict();
        current.setDictType(DICT_TYPE).setDictKey(repository.getStorageIdAndRepositoryId()).setCreateTime(date);
        dictService.saveDict(current);
        if(Objects.nonNull(updates)&&!updates.isEmpty()){
            log.info("开始备份");
            // 1.压缩要备份的文件 2.是否存在同名的raw仓库 3.存入对应仓库
            String path=dateStr+"/"+"backUp.zip";
            replicationBackup.backUpByPath(repository,updates,path);
        }
    }

    private void syncAllPackagesGz(Repository repository, String remoteUrl, Path localDir, String preDate, String currentDate, Set<String> updates) {
        // 下载当前目录的 HTML 页面
        String html = artifactComponent.getHtml(repository, remoteUrl);
        // 用正则解析所有 <a href="..."> 链接
        Pattern pattern = Pattern.compile("<a href=\"([^\"]+)\">");
        Matcher matcher = pattern.matcher(html);
        Set<String> links = new HashSet<>();
        while (matcher.find()) {
            String link = matcher.group(1);
            // 忽略上级目录链接 "../"
            if (link.equals("../")) continue;
            links.add(link);
        }
        // 遍历所有链接
        for (String link : links) {
            String fullUrl = remoteUrl + link;
            if (link.endsWith("/")) {
                // 目录：递归调用
                syncAllPackagesGz(repository, fullUrl, localDir, preDate, currentDate, updates);
            } else if (link.contains("Packages.gz")) {
                String relative = StringUtils.removeStart(fullUrl, repository.getRemoteRepository().getUrl());
                String relativePath = StringUtils.removeStart(relative, "/");
                syncSpecificPackagesGz(repository, relativePath, localDir.toString(), preDate, currentDate, updates);
            }
        }
    }

    private void syncSpecificPackagesGz(Repository repository, String packageGzPath, String distDir, String preDate, String currentDate, Set<String> updates) {
        Matcher matcher = DebianConstant.PACKAGE_PATTERN.matcher(packageGzPath);
        // 将文件下载到本地
        if (matcher.matches()) {
            String codename = matcher.group("codename");
            String component = matcher.group("component");
            String architecture = matcher.group("architecture");
            Set<String> previous = new HashSet<>();
            try {
                if (Objects.nonNull(preDate)) {
                    String preDist = distDir + "/" + preDate + "/" + getPackageNameByPath(codename, component, architecture);
                    previous = parsePackagesFile(readGzipFile(Path.of(preDist)));
                }
                String currentDist = distDir + "/" + currentDate + "/" + getPackageNameByPath(codename, component, architecture);
                String fullPath = StringUtils.removeEnd(repository.getRemoteRepository().getUrl(), "/") + "/" + packageGzPath;
                artifactComponent.getArtifactByUrl(repository, fullPath, currentDist);
                // 获取本次的package.gz
                Set<String> current = parsePackagesFile(readGzipFile(Path.of(currentDist)));
                Set<String> diff = new HashSet<>(current);
                diff.removeAll(previous);
                // 同步制品
                for (String item : diff) {
                    String artifactPath = item + ";" + DebianUtils.getArrtString(codename, component, architecture);
                    artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), artifactPath);
                }
                if (Objects.nonNull(updates)) {
                    updates.addAll(diff);
                }
                artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), packageGzPath);
                String releasePath = "dists/" + codename + "/Release";
                artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), releasePath);
            } catch (Exception e) {
                log.error("同步发行版【{}】,组件【{}】,架构【{}】时异常", codename, component, architecture, e);
            }
        }
    }

    private static Set<String> parsePackagesFile(String packagesText) {
        Set<String> packageFiles = new HashSet<>();
        // 每个包信息块通常以空行分隔，查找 "Filename:" 开头的行
        // 示例行：Filename: pool/main/b/bash/bash_5.0-4ubuntu1_amd64.deb
        Pattern pattern = Pattern.compile("Filename:\\s*(\\S+)");
        Matcher matcher = pattern.matcher(packagesText);
        while (matcher.find()) {
            String filename = matcher.group(1);
            packageFiles.add(filename);
        }
        return packageFiles;
    }

    private String readGzipFile(Path gzFilePath) throws IOException {
        try (InputStream fis = Files.newInputStream(gzFilePath);
             GZIPInputStream gis = new GZIPInputStream(fis);
             ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            byte[] buffer = new byte[4096];
            int len;
            while ((len = gis.read(buffer)) != -1) {
                baos.write(buffer, 0, len);
            }
            return baos.toString(StandardCharsets.UTF_8);
        } catch (Exception e) {
            return "";
        }
    }

    String getPackageNameByPath(String codename, String component, String architecture) {
        return codename + "-" + component + "-" + architecture + "-Packages.gz";
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(SyncRemoteDebianCronJob.class.getName())
                .name("debian仓库全量同步及定时增量任务").scope(DEBIAN)
                .description("该任务用于全量同步远程debian仓库及定时增量同步")
                .fields(FIELDS)
                .build();
    }
}
