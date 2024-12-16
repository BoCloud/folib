package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.indexer.HelmMetadataIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;
import java.util.Set;

public class RemoveHelmIndexCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";
    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";
    private static final String INDEX_FILE = "index.yaml";
    private static final String CHARTS_INDEX_FILE = "charts/index.yaml";

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ArtifactManagementService artifactManagementService;

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));


    @Override
    protected void executeTask(CronTaskConfigurationDto config) throws Throwable {

        if (config == null) {
            throw new IllegalArgumentException("Config cannot be null");
        }
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        if (storageId == null || repositoryId == null) {
            return;
        }

        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        if (repository != null && RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            RepositoryPath trashPath = repositoryPathResolver.resolve(storageId, repositoryId, INDEX_FILE);
            RepositoryPath trashPath2 = repositoryPathResolver.resolve(storageId, repositoryId, CHARTS_INDEX_FILE);
            if (Files.exists(trashPath)) {
                Files.deleteIfExists(trashPath);
                artifactResolutionService.resolvePath(storageId, repositoryId, INDEX_FILE);
            } else if (Files.exists(trashPath2)) {
                Files.deleteIfExists(trashPath2);
                artifactResolutionService.resolvePath(storageId, repositoryId, CHARTS_INDEX_FILE);
            } else {
                RepositoryPath path = artifactResolutionService.resolvePath(storageId, repositoryId, INDEX_FILE);
                if (path == null) {
                    path = artifactResolutionService.resolvePath(storageId, repositoryId, CHARTS_INDEX_FILE);
                }
                if (path == null) {
                   logger.warn("未找到对应的{} {} index.yaml文件",storageId, repositoryId);
                }
            }
        } else if (repository != null && RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            HelmMetadataIndexer indexer = new HelmMetadataIndexer(storageId, repositoryId, artifactManagementService, repositoryPathResolver);
            indexer.reindexAsSystem();
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveHelmIndexCronJob.class.getName())
                .name("定时更新helm的index.yaml任务").scope(HELM)
                .description("该任务用于定时更新helm库的index.yaml")
                .fields(FIELDS)
                .build();
    }

    private Map<String, Storage> getStorages() {
        return configurationManager.getConfiguration().getStorages();
    }
}
