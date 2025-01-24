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
import com.veadan.folib.services.DictService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;
import java.util.Set;

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

    private final String DICT_TYPE="repository_replication_task";


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
        Dict lasted= dictService.selectLatestOneDict(query);
        // 没有同步过 开始全量同步
        replication(repository,lasted);
    }

    void replication(Repository repository,Dict dict){
        String distPath=tempPath+"/replication/"+repository.getStorage().getId()+"/"+repository.getId();
        if(dict==null){
            String repomdUrl=repository.getRemoteRepository().getUrl()+"/repodata/repomd.xml";
            artifactComponent.getArtifactByUrl(repository,repomdUrl,distPath);
        }

        // 1.获取repomd.xml文件去找  2.获取primary.xml.gz  3.解析文件并下载包4.更新数据
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(SyncRemoteRpmCronJob.class.getName())
                .name("仓库全量同步及定时增量任务").scope(RPM)
                .description("该任务用于全量同步远程rpm仓库及定时增量同步")
                .fields(FIELDS)
                .build();
    }
}
