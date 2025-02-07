package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobField;
import com.veadan.folib.cron.jobs.fields.CronJobNamedField;
import com.veadan.folib.cron.jobs.fields.CronJobOptionalField;
import com.veadan.folib.cron.jobs.fields.CronJobRepositoryIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStorageIdAutocompleteField;
import com.veadan.folib.cron.jobs.fields.CronJobStringTypeField;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.metadata.indexer.RpmRepoIndexer;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.annotation.Resource;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.zip.GZIPInputStream;

/**
 * @author huayanjun
 * @since 2025-01-22 10:25
 */
@Slf4j
public class SyncRemoteRpmCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

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

    private final String DICT_TYPE = "repository_replication_task";


    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

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
        replication(repository, lasted);
    }

    void replication(Repository repository, Dict dict) {
        String repomDistPath = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId() + "/repomd.xml";
        String repomdUrl = repository.getRemoteRepository().getUrl() + "/repodata/repomd.xml";
        artifactComponent.getArtifactByUrl(repository, repomdUrl, repomDistPath);
        try {
            Dict newDict = extractPrimaryXmlPath(new FileInputStream(repomDistPath), repository);
            if (Objects.isNull(newDict)) {
                return;
            }
            dictService.saveDict(newDict);
            if (dict == null || !dict.getDictValue().equals(newDict.getDictValue())) {
                log.info("开始获取新制品");
                String primaryDistPath = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId() + "/" + newDict.getDictValue();
                String primaryXmlUrl = repository.getRemoteRepository().getUrl() + "/" + newDict.getDictValue();
                artifactComponent.getArtifactByUrl(repository, primaryXmlUrl, primaryDistPath);
                // 解析仓库里的制品
                List<String> previous = new ArrayList<>();
                if (dict != null) {
                    String prePath = tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId() + "/" + dict.getDictValue();
                    previous = parsePrimaryXml(Paths.get(prePath));
                }
                List<String> current = parsePrimaryXml(Paths.get(primaryDistPath));
                List<String> diff = new LinkedList<>(current);
                diff.removeAll(previous);
                for (String path : diff) {
                    artifactResolutionService.resolvePath(repository.getStorage().getId(), repository.getId(), path);
                }
                log.info("同步制品完成，开始更新索引");
                RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
                rpmRepoIndexer.indexWriter(repository);
                log.info("更新索引完成");
            }
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
    private Dict extractPrimaryXmlPath(InputStream repomdXml, Repository repository) throws Exception {
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document doc = builder.parse(repomdXml);
        NodeList nodes = doc.getElementsByTagName("data");
        for (int i = 0; i < nodes.getLength(); i++) {
            Element dataElement = (Element) nodes.item(i);
            if ("primary".equals(dataElement.getAttribute("type"))) {
                String primaryXmlPath = dataElement.getElementsByTagName("location").item(0).getAttributes()
                        .getNamedItem("href").getNodeValue();
                Dict update = new Dict();
                update.setDictType(DICT_TYPE).setDictKey(repository.getStorageIdAndRepositoryId()).setDictValue(primaryXmlPath);
                return update;
            }
        }
        return null;
    }

    private List<String> parsePrimaryXml(Path primaryXmlPath) throws Exception {
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
