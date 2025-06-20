package com.veadan.folib.job.tasks;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.job.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.job.cron.jobs.CronJobDefinition;
import com.veadan.folib.job.cron.jobs.JavaCronJob;
import com.veadan.folib.job.cron.jobs.fields.*;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.services.PubArtifactIndexService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.inject.Inject;
import java.util.Map;
import java.util.Set;

/**
 * @author leipenghui
 **/
@Slf4j
public class RebuildPubMetadataCronJob extends JavaCronJob {

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
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private PubArtifactIndexService artifactIndexService;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            log.info("Pub index job single repository [{}] [{}]", storageId, repositoryId);
            pubIndexRepository(storageId, repositoryId);
        } else {
            log.info("Pub index job all repository");
            pubIndexRepository();
        }
    }

    public void pubIndexRepository(String storageId, String repositoryId) {
        log.info("Start pub index job repository [{}] [{}]", storageId, repositoryId);
        Repository repository = configurationManagementService.getConfiguration().getStorage(storageId).getRepository(repositoryId);
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }
        artifactIndexService.rebuildIndex(storageId, repositoryId, "");
    }

    public void pubIndexRepository() {
        try {
            for (Map.Entry<String, Storage> entry : configurationManagementService.getConfiguration().getStorages().entrySet()) {
                try {
                    Storage storage = entry.getValue();
                    final Map<String, ? extends Repository> repositories = storage.getRepositories();
                    for (Repository repository : repositories.values()) {
                        try {
                            if (PubLayoutProvider.ALIAS.equals(repository.getLayout()) && RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                                pubIndexRepository(repository.getStorage().getId(), repository.getId());
                            }
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    }
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RebuildPubMetadataCronJob.class.getName())
                .name("定时重建Pub仓库的索引任务")
                .scope(PUB)
                .description("该任务用于定时重建Pub仓库制品包的索引任务")
                .fields(FIELDS)
                .build();
    }
}