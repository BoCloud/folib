package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.metadata.indexer.RpmRepoIndexer;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;

import javax.inject.Inject;
import java.util.Objects;
import java.util.Set;

/**
 * @author leipenghui
 * @date 2024/12/10
 **/
@Slf4j
public class RebuildRpmMetadataCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private ConfigurationManager configurationManager;

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
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
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            log.warn("Repository storageId [{}] repositoryId [{}] not is hosted type skip..", storageId, repositoryId);
            return;
        }
        RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
        rpmRepoIndexer.indexWriter(repository);
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RebuildRpmMetadataCronJob.class.getName())
                .name("定时重建Rpm仓库的索引任务").scope(RPM)
                .description("该任务用于定时重建Rpm仓库制品包的索引任务")
                .fields(FIELDS)
                .build();
    }
}

