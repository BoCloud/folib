package com.veadan.folib.cron.jobs;

import com.google.common.collect.ImmutableSet;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.*;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repositories.VulnerabilityRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import javax.inject.Inject;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 */
@Slf4j
public class CleanupInvalidIndexesCronJob
        extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))));

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private VulnerabilityRepository vulnerabilityRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManager configurationManager;


    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);

        if (storageId == null && repositoryId == null) {
            cleanup();
        } else {
            cleanup(storageId, repositoryId);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(CleanupInvalidIndexesCronJob.class.getName())
                .name("仓库失效索引定时清理任务")
                .scope(GLOBAL)
                .description("该任务可定时删除制品仓库下的失效索引数据")
                .fields(FIELDS)
                .build();
    }

    private void cleanup() {
        for (Map.Entry<String, Storage> entry : configurationManager.getConfiguration().getStorages().entrySet()) {
            try {
                Storage storage = entry.getValue();
                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values()) {
                    try {
                        logger.info("Cleanup invalid index for storageId [{}] repositoryId [{}]", storage.getId(), repository.getId());
                        cleanup(storage.getId(), repository.getId());
                    } catch (Exception ex) {
                        logger.warn(ExceptionUtils.getStackTrace(ex));
                    }
                }
            } catch (Exception ex) {
                logger.warn(ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private void cleanup(String storageId, String repositoryId) {
        Storage storage = configurationManager.getStorage(storageId);
        if (Objects.nonNull(storage)) {
            Repository repository = storage.getRepository(repositoryId);
            if (Objects.nonNull(repository) && !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                return;
            }
        }
        long totalCount = artifactRepository.artifactsCount(storageId, repositoryId);
        if (totalCount <= 0) {
            return;
        }
        int batchSize = 100;
        // 计算总页数
        int totalPages = (int) Math.ceil((double) totalCount / batchSize);
        Pageable pageable;
        String artifactPath = "";
        Page<Artifact> page;
        List<Artifact> artifactList;
        RepositoryPath repositoryPath;
        for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
            log.info("TotalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
            if (currentPage == 1) {
                pageable = PageRequest.of(currentPage, batchSize).first();
            } else {
                pageable = PageRequest.of(currentPage, batchSize).previous();
            }
            page = artifactRepository.findMatchingByIndex(pageable, false, null, null, storageId, repositoryId, null, null, null, null, null, null, null);
            if (CollectionUtils.isNotEmpty(page.getContent())) {
                artifactList = page.getContent();
                for (Artifact artifact : artifactList) {
                    try {
                        if (LocalDateTime.now().minusMinutes(10).isBefore(artifact.getCreated())) {
                            continue;
                        }
                        artifactPath = artifact.getArtifactCoordinates().buildPath();
                        repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                        if (!Files.exists(repositoryPath)) {
                            artifactRepository.delete(artifact);
                            if (CollectionUtils.isNotEmpty(artifact.getVulnerabilities())) {
                                vulnerabilityRepository.handlerVulnerabilityForArtifactDelete(repositoryPath, artifact.getVulnerabilities());
                            }
                            log.info("Cleanup invalid index storageId [{}] repositoryId [{}] artifactPath [{}] deleted", artifact.getStorageId(), artifact.getRepositoryId(), artifactPath);
                        }
                    } catch (Exception ex) {
                        log.error("Cleanup invalid index storageId [{}] repositoryId [{}] artifactPath [{}] error [{}]", artifact.getStorageId(), artifact.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
                    }
                }
            }
        }
    }
}
