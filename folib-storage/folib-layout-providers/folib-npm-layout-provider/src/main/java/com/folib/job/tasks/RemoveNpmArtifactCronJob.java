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
import com.github.zafarkhaja.semver.Version;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.folib.constant.GlobalConstants;
import com.folib.job.cron.domain.CronTaskConfigurationDto;
import com.folib.job.cron.jobs.CronJobDefinition;
import com.folib.job.cron.jobs.JavaCronJob;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
public class RemoveNpmArtifactCronJob extends JavaCronJob {

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_BASE_PATH = "basePath";

    private static final String PROPERTY_NUMBER_TO_KEEP = "numberToKeep";

    private static final Set<CronJobField> FIELDS = ImmutableSet.of(
            new CronJobStorageIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_STORAGE_ID)))),
            new CronJobRepositoryIdAutocompleteField(new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobNamedField(PROPERTY_REPOSITORY_ID)))),
            new CronJobStringTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_BASE_PATH), "基础路径"))),
            new CronJobIntegerTypeField(
                    new CronJobOptionalField(new CronJobAliasNamedField(new CronJobNamedField(PROPERTY_NUMBER_TO_KEEP), "保留版本"))));

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public void executeTask(CronTaskConfigurationDto config)
            throws Throwable {
        String storageId = config.getProperty(PROPERTY_STORAGE_ID);
        String repositoryId = config.getProperty(PROPERTY_REPOSITORY_ID);
        String basePath = config.getProperty(PROPERTY_BASE_PATH);
        String numberToKeep = config.getProperty(PROPERTY_NUMBER_TO_KEEP);
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId) && StringUtils.isNotBlank(numberToKeep)) {
            deleteNpmArtifact(storageId, repositoryId, basePath, Integer.parseInt(numberToKeep));
        }
    }

    public void deleteNpmArtifact(String storageId, String repositoryId, String basePath, int numberToKeep) {
        log.info("Remove npm artifact job repository [{}] [{}] basePath [{}] numberToKeep [{}]", storageId, repositoryId, basePath, numberToKeep);
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (Objects.isNull(storage)) {
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            return;
        }
        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(storageId, repositoryId);
        if (!Files.exists(repositoryBasePath)) {
            return;
        }
        if (StringUtils.isNotBlank(basePath)) {
            repositoryBasePath = repositoryBasePath.resolve(basePath);
        }
        if (!Files.exists(repositoryBasePath)) {
            return;
        }
        List<String> repositoryPathList = getNpmPackagePaths(repositoryBasePath);
        if (CollectionUtils.isEmpty(repositoryPathList)) {
            return;
        }
        for (String npmPackageName : repositoryPathList) {
            execute(storageId, repositoryId, npmPackageName, numberToKeep);
        }
    }

    private void execute(String storageId, String repositoryId, String npmPackageName, int numberToKeep) {
        try {
            RepositoryPath npmPackageRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, npmPackageName);
            if (!Files.exists(npmPackageRepositoryPath)) {
                return;
            }
            List<String> npmVersionPathList = Lists.newArrayList();
            try (Stream<Path> npmPathStream = Files.list(npmPackageRepositoryPath)) {
                npmPathStream.filter(Files::isDirectory).forEach(npmVersion -> {
                    RepositoryPath npmVersionRepositoryPath = (RepositoryPath) npmVersion;
                    boolean isNpmVersion = false;
                    String version = npmVersionRepositoryPath.getFileName().toString();
                    try {
                        Version.valueOf(version);
                        isNpmVersion = true;
                    } catch (Exception e) {
                        log.warn("Version {} is not a valid semver 2 version, version will not be added", version, e);
                    }
                    logger.info("Remove npm artifact job storageId [{}] repositoryId [{}] package [{}] path [{}] isNpmVersion [{}]", storageId, repositoryId, npmPackageName, version, isNpmVersion);
                    if (isNpmVersion) {
                        npmVersionPathList.add(npmVersion.getFileName().toString());
                    }
                });
            } catch (IOException ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
            sort(npmVersionPathList);
            int size = npmVersionPathList.size();
            if (numberToKeep > 0 && size > 1 && size > numberToKeep) {
                List<String> removeNpmVersionList = npmVersionPathList.subList(0, size - numberToKeep);
                logger.info("Remove npm artifact job storageId [{}] repositoryId [{}] package [{}] version count [{}] numberToKeep [{}] versions [{}] delete [{}]", storageId, repositoryId, npmPackageName, size, numberToKeep, String.join(",", npmVersionPathList), String.join(",", removeNpmVersionList));
                for (String versionPath : removeNpmVersionList) {
                    try {
                        RepositoryPath versionDirectoryPath = npmPackageRepositoryPath.resolve(versionPath);
                        logger.info("Delete npm artifact path [{}]", versionDirectoryPath.toString());
                        RepositoryFiles.delete(versionDirectoryPath, true);
                    } catch (Exception ex) {
                        log.error(ExceptionUtils.getStackTrace(ex));
                    }
                }
            } else {
                logger.info("Remove npm artifact job storageId [{}] repositoryId [{}] package [{}] version count [{}] numberToKeep [{}] skip..", storageId, repositoryId, npmPackageName, size, numberToKeep);
            }
        } catch (Exception ex) {
            log.error("Remove npm artifact job repository [{}] [{}] numberToKeep [{}] package [{}] error [{}]", storageId, repositoryId, numberToKeep, npmPackageName, ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 获取绝对路径下的所有文件
     *
     * @param repositoryPath 路径
     */
    public List<String> getNpmPackagePaths(RepositoryPath repositoryPath) {
        List<String> pathList = Lists.newArrayList();
        if (!Files.exists(repositoryPath)) {
            log.warn("Path [{}] not exists", repositoryPath);
            return pathList;
        }
        if (!Files.isDirectory(repositoryPath)) {
            log.warn("Path [{}] not is directory", repositoryPath);
            return pathList;
        }
        try {
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    log.debug("Current path [{}]", itemPath);
                    if (Files.isSameFile(itemPath.getParent(), itemPath.getRoot())) {
                        return FileVisitResult.CONTINUE;
                    }
                    RepositoryPath parentRepositoryPath = itemPath.getParent().getParent();
                    if (!isArtifactDirectory(parentRepositoryPath)) {
                        return FileVisitResult.CONTINUE;
                    }
                    String artifactPath = RepositoryFiles.relativizePath(parentRepositoryPath);
                    if (StringUtils.isNotBlank(artifactPath) && RepositoryPathUtil.include(1, itemPath, false, "") && !pathList.contains(artifactPath)) {
                        log.debug("Find npm path [{}]", itemPath);
                        pathList.add(artifactPath);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) dir;
                    log.debug("Current directory path [{}]", itemPath);
                    if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, false, "")) {
                        log.debug("RepositoryPath [{}] skip...", itemPath.toString());
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                                                          IOException exc)
                        throws IOException {
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
        log.info("Path [{}] find npm paths [{}]", repositoryPath, pathList.size());
        return pathList;
    }

    private void sort(List<String> packageVersionList) {
        filterInvalidVersions(packageVersionList);
        packageVersionList.sort((o1, o2) -> Version.BUILD_AWARE_ORDER.compare(Version.valueOf(o1), Version.valueOf(o2)));
    }

    private void filterInvalidVersions(List<String> packageVersionList) {
        for (String version : packageVersionList) {
            try {
                Version.valueOf(version);
            } catch (Exception e) {
                packageVersionList.remove(version);
                log.warn("Version {} is not a valid semver 2 version, version will not be added", version, e);
            }
        }
    }

    public boolean isArtifactDirectory(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length <= 2) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    @Override
    public CronJobDefinition getCronJobDefinition() {
        return CronJobDefinition.newBuilder()
                .jobClass(RemoveNpmArtifactCronJob.class.getName())
                .name("定时删除Npm制品任务")
                .scope(NPM)
                .description("该任务用于按照版本数量清理Npm制品包")
                .fields(FIELDS)
                .build();
    }
}