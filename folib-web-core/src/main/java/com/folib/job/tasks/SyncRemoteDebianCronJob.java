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
package com.folib.job.tasks;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.google.common.collect.ImmutableSet;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.DebianConstant;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.job.cron.jobs.fields.CronJobAliasNamedField;
import com.folib.job.cron.jobs.fields.CronJobBooleanTypeField;
import com.folib.job.cron.jobs.fields.CronJobField;
import com.folib.job.cron.jobs.fields.CronJobNamedField;
import com.folib.job.cron.jobs.fields.CronJobOptionalField;
import com.folib.job.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.folib.job.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.folib.job.cron.jobs.fields.CronJobStringTypeField;
import com.folib.entity.Dict;
import com.folib.indexer.DebianReleaseMetadataIndexer;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.DictService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.DebianUtils;
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
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
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
    private RepositoryPathResolver repositoryPathResolver;

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
        Set<String> increase=new HashSet<>();
        if (scope == null) {
            String remoteUrl = StringUtils.removeEnd(repository.getRemoteRepository().getUrl(), "/") + "/dists/";
            increase=syncAllPackagesGz(repository, remoteUrl, Path.of(tempDir), preDate, dateStr, backup);
        } else {
            String[] groups = scope.split(",");
            for (String group : groups) {
                String[] comp = group.split(":");
                if (comp.length != 3) {
                    log.error("无效的组件格式");
                    continue;
                }
                String packagesGzPath = String.format("dists/%s/%s/binary-%s/Packages.gz", comp[0], comp[1], comp[2]);
                Set<String> part = syncSpecificPackagesGz(repository, packagesGzPath, tempDir, preDate, dateStr, backup);
                increase.addAll(part);
            }
        }
        for (String path : increase) {
            try {
                artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), path);
            } catch (IOException e) {
                log.info("下载包异常{}",path);
            }
        }
        Dict current = new Dict();
        current.setDictType(DICT_TYPE).setDictKey(repository.getStorageIdAndRepositoryId()).setCreateTime(date);
        log.info("debian replication [{}] [{}] 完成", repository.getStorage().getId(), repository.getId() );
        dictService.saveDict(current);
        if (!increase.isEmpty()&&backup) {
            log.info("开始备份");
            // 1.压缩要备份的文件 2.是否存在同名的raw仓库 3.存入对应仓库
            String path = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATETIME_PATTERN) + "/backup/";
            replicationBackup.backUpByPath(repository, increase, path);
        }
    }

    private Set<String> syncAllPackagesGz(Repository repository, String remoteUrl, Path localDir, String preDate, String currentDate, boolean backup) {
        // 下载当前目录的 HTML 页面
        String html = artifactComponent.getHtml(repository, remoteUrl);
        // 用正则解析所有 <a href="..."> 链接
        Pattern pattern = Pattern.compile("<a href=\"([^\"]+)\">");
        Matcher matcher = pattern.matcher(html);
        Set<String> links = new HashSet<>();
        Set<String> increase = new HashSet<>();
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
                syncAllPackagesGz(repository, fullUrl, localDir, preDate, currentDate, backup);
            } else if (link.contains("Packages.gz")) {
                String relative = StringUtils.removeStart(fullUrl, repository.getRemoteRepository().getUrl());
                String relativePath = StringUtils.removeStart(relative, "/");
                Set<String> part = syncSpecificPackagesGz(repository, relativePath, localDir.toString(), preDate, currentDate, backup);
                increase.addAll(part);
            }
        }
        return increase;
    }

    private Set<String> syncSpecificPackagesGz(Repository repository, String packageGzPath, String distDir, String preDate, String currentDate, boolean backup)
    {
        Matcher matcher = DebianConstant.PACKAGE_PATTERN.matcher(packageGzPath);
        // 将文件下载到本地
        Set<String> newList = new HashSet<>();
        if (matcher.matches()) {
            String codename = matcher.group("codename");
            String component = matcher.group("component");
            String architecture = matcher.group("architecture");
            try {
                String currentDist = distDir + "/" + currentDate + "/" + getPackageNameByPath(codename, component, architecture);
                String fullPath = StringUtils.removeEnd(repository.getRemoteRepository().getUrl(), "/") + "/" + packageGzPath;
                artifactComponent.getArtifactByUrl(repository, fullPath, currentDist);
                // 获取本次的package.gz
                Set<String> current = parsePackagesFile(readGzipFile(Path.of(currentDist)));
                // 同步制品
                for (String item : current) {
                    String artifactPath = item + ";" + DebianUtils.getArrtString(codename, component, architecture);
                    try {
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
                        if(!backup){
                            artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), artifactPath);
                        }
                        if(!Files.exists(repositoryPath)&&backup) {
                            newList.add(item);
                        }
                    } catch (Exception e) {
                        log.error("resolve path [{}] failed", item, e);
                    }
                }

                //
                if(!newList.isEmpty()){
                    newList.add(packageGzPath);
                }
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageGzPath);
                Files.delete(repositoryPath);
                artifactResolutionService.resolvePath(repositoryPath);
                // 将package 文件写入其中
                (new DebianReleaseMetadataIndexer(repository, Collections.emptyList(), repositoryPathResolver)).indexRelease(codename);
                return newList;
            } catch (Exception e) {
                log.error("同步发行版【{}】,组件【{}】,架构【{}】时异常", codename, component, architecture, e);
            }
        }
        return newList;
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
