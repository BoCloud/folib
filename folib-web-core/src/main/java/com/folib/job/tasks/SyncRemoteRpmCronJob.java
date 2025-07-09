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

import com.google.common.collect.ImmutableSet;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.configuration.ConfigurationManager;
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
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.DictService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.annotation.Resource;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @since 2025-01-22 10:25
 */
@Slf4j
public class SyncRemoteRpmCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String BACKUP_INCREASE = "backupIncrease";

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
    private RepositoryPathResolver repositoryPathResolver;

    @Resource
    private ArtifactManagementService artifactManagementService;


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
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
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
        replication(repository, lasted, backup);
    }

    void replication(Repository repository, Dict dict, boolean backup) {
        String repomDistPath = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId() + "/repomd.xml";
        String repomdUrl = repository.getRemoteRepository().getUrl() + "/repodata/repomd.xml";
        artifactComponent.getArtifactByUrl(repository, repomdUrl, repomDistPath);
        try {
            List<String> packageList=new ArrayList<>();
            packageList.add("repodata/repomd.xml");
            Dict newDict = extractPrimaryXmlPath(new FileInputStream(repomDistPath), repository,packageList);
            if (Objects.isNull(newDict)) {
                return;
            }
            if (newDict != null ) {
                log.info("开始获取新制品");
                String primaryDistPath = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId() + "/" + newDict.getDictValue();
                String primaryXmlUrl = repository.getRemoteRepository().getUrl() + "/" + newDict.getDictValue();
                artifactComponent.getArtifactByUrl(repository, primaryXmlUrl, primaryDistPath);
                List<String> current = parsePrimaryXml(Paths.get(primaryDistPath));
                List<String> newList = new ArrayList<>();
                for (String path : current) {
                    try {
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, path);
                        if(!Files.exists(repositoryPath)&&backup) {
                            newList.add(path);
                            artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), path);
                        }

                    } catch (Exception e) {
                        log.error("resolve path [{}] failed", path, e);
                    }
                }
                log.info("更新索引完成");
                if (backup&& !newList.isEmpty()) {
                    log.info("开始备份");
                    newList.addAll(packageList);
                    // 1.压缩要备份的文件 2.是否存在同名的raw仓库 3.存入对应仓库
                    String date= DATE_FORMAT.format(new Date());
                    String path=date+"/backup/";
                    replicationBackup.backUpByPath(repository,newList,path);
                }
            }
            dictService.saveDict(newDict);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(SyncRemoteRpmCronJob.class.getName())
                .name("rpm仓库全量同步及定时增量任务").scope(RPM)
                .description("该任务用于全量同步远程rpm仓库及定时增量同步")
                .fields(FIELDS)
                .build();
    }


    // 解析repomd.xml文件获取保存check
    private Dict extractPrimaryXmlPath(InputStream repomdXml, Repository repository, List<String> packageList) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        // 禁用各种可能引发 XXE 的功能
        factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
        factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
        factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
        factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
        factory.setXIncludeAware(false);
        factory.setExpandEntityReferences(false);

        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(new InputSource(repomdXml));

        NodeList nodes = doc.getElementsByTagName("data");
        Dict update = null;
        for (int i = 0; i < nodes.getLength(); i++) {
            Element dataElement = (Element) nodes.item(i);
            String localPath = dataElement.getElementsByTagName("location").item(0).getAttributes()
                    .getNamedItem("href").getNodeValue();
            artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), localPath);
            packageList.add(localPath);
            if ("primary".equals(dataElement.getAttribute("type"))) {
                update = new Dict();
                update.setDictType(DICT_TYPE)
                        .setDictKey(repository.getStorageIdAndRepositoryId())
                        .setDictValue(localPath);
            }
        }

        RepositoryPath repomd = repositoryPathResolver.resolve(repository, "repodata/repomd.xml");
        if (Files.exists(repomd)) {
            Files.delete(repomd);
        }
        artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), "repodata/repomd.xml");

        return update;
    }

    private List<String> parsePrimaryXml(Path primaryXmlPath) {
        List<String> rpmFiles = new ArrayList<>();
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(Files.newInputStream(primaryXmlPath))) {
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document doc = builder.parse(gzipInputStream);
            NodeList packageNodes = doc.getElementsByTagName("package");
            for (int i = 0; i < packageNodes.getLength(); i++) {
                Element packageElement = (Element) packageNodes.item(i);
                Element locationElement = (Element) packageElement.getElementsByTagName("location").item(0);
                String rpmPath = locationElement.getAttribute("href");
                rpmFiles.add(rpmPath);
            }
            return rpmFiles;
        } catch (Exception e) {
            return rpmFiles;
        }
    }




}
