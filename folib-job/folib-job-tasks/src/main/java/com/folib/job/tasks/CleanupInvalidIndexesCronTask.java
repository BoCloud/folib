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

import com.folib.job.cron.jobs.fields.*;
import com.google.common.collect.ImmutableSet;
import com.folib.configuration.ConfigurationManager;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.domain.CronTasksConfigurationDto;
import com.folib.domain.Artifact;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.repositories.VulnerabilityRepository;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import jakarta.inject.Inject;
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
public class CleanupInvalidIndexesCronTask
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
                .jobClass(CleanupInvalidIndexesCronTask.class.getName())
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
                        if(!existsRepositoryTask(storage.getId(), repository.getId())){
                            cleanup(storage.getId(), repository.getId());
                        }
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
            page = artifactRepository.findMatchingByIndex(pageable, false, null, null, storageId, repositoryId, null, null, null, null, null, null, null, null, null, null);
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
                            artifactRepository.delete(artifact, repositoryPath.getRepository().getLayout());
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

    private boolean existsRepositoryTask(String storageId, String repositoryId) {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (CollectionUtils.isEmpty(config.getCronTaskConfigurations())) {
            return false;
        }
        String cronJob = "com.folib.cron.jobs.cleanup.CleanupInvalidIndexesCronJob";
        return config.getCronTaskConfigurations().stream().anyMatch(cron -> storageId.equals(cron.getProperty("storageId")) && repositoryId.equals(cron.getProperty("repositoryId")) && cronJob.equals(cron.getJobClass()));
    }
}
