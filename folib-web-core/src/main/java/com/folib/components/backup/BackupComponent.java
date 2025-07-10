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
package com.folib.components.backup;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/12/19
 **/
@Slf4j
@Component
public class BackupComponent {

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private ConfigurationManager configurationManager;

    public void backupRepositories(String strategyName, String backupPath, boolean incremental, List<String> repositories) throws Exception {
        if (StringUtils.isBlank(strategyName) || StringUtils.isBlank(backupPath) || CollectionUtils.isEmpty(repositories)) {
            return;
        }
        String storageId, repositoryId;
        for (String storageIdAndRepositoryId : repositories) {
            storageId = ConfigurationUtils.getStorageId("", storageIdAndRepositoryId);
            if (StringUtils.isBlank(storageId)) {
                log.warn("StorageId is null skip...");
                continue;
            }
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                log.warn("StorageId [{}] non existent skip...", storageId);
                continue;
            }
            repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
            if (StringUtils.isBlank(repositoryId)) {
                log.warn("StorageId [{}] repositoryId is null skip...", storageId);
                continue;
            }
            Repository repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                log.warn("Repository [{}] repositoryId [{}] non existent skip...", storageId, repositoryId);
                continue;
            }
            backupRepository(storageId, repositoryId, repository, strategyName, backupPath, incremental);
        }
    }

    private void backupRepository(String storageId, String repositoryId, Repository repository, String strategyName, String backupPath, boolean incremental) throws IOException {
        RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
        if (!Files.exists(rootRepositoryPath)) {
            log.warn("Repository [{}] repositoryId [{}] path non existent skip...", storageId, repositoryId);
            return;
        }
        Path targetParentPath = getTargetParentPath(storageId, repositoryId, strategyName, backupPath, incremental);
        Files.walkFileTree(rootRepositoryPath, EnumSet.of(FileVisitOption.FOLLOW_LINKS), 2147483647, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                try {
                    if (ignoreDirectory(repository, (RepositoryPath) dir)) {
                        log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] directory [{}] ignore skip...", strategyName, storageId, repositoryId, dir);
                        return FileVisitResult.SKIP_SUBTREE;
                    }
                    String path = RepositoryFiles.relativizePath((RepositoryPath) dir);
                    if (StringUtils.isBlank(path)) {
                        return FileVisitResult.CONTINUE;
                    }
                    Path targetPath = targetParentPath.resolve(path);
                    boolean existsBackup = incremental && Files.exists(targetPath);
                    if (existsBackup) {
                        log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] directory [{}] exists skip...", strategyName, storageId, repositoryId, path);
                        return FileVisitResult.CONTINUE;
                    }
                    Path backupDirectory = Files.createDirectories(targetPath);
                    log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] directory [{}] successful", strategyName, storageId, repositoryId, backupDirectory);
                } catch (NoSuchFileException e) {
                    // 文件已删除，跳过处理
                    log.warn("Backup strategyName [{}] storageId [{}] repositoryId [{}] directory [{}] no such directory skip...", strategyName, storageId, repositoryId, dir);
                    return FileVisitResult.CONTINUE;
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                if (exc instanceof NoSuchFileException) {
                    // 目录或文件已删除，继续遍历
                    log.warn("Backup strategyName [{}] storageId [{}] repositoryId [{}] directory [{}] no such file skip...", strategyName, storageId, repositoryId, file);
                    return FileVisitResult.CONTINUE;
                }
                return super.visitFileFailed(file, exc);
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                try {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    if (ignorePath(repository, itemPath)) {
                        log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] path [{}] ignore skip...", strategyName, storageId, repositoryId, itemPath);
                        return FileVisitResult.CONTINUE;
                    }
                    String path = RepositoryFiles.relativizePath(itemPath);
                    if (StringUtils.isBlank(path)) {
                        return FileVisitResult.CONTINUE;
                    }
                    Path targetPath = targetParentPath.resolve(path);
                    boolean existsBackup = incremental && Files.exists(targetPath) && (RepositoryFiles.validateChecksum(itemPath, targetPath) || DockerLayoutProvider.ALIAS.equals(repository.getLayout()));
                    if (existsBackup) {
                        log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] path [{}] exists skip...", strategyName, storageId, repositoryId, path);
                        return FileVisitResult.CONTINUE;
                    }
                    Path backupPath = Files.copy(file, targetPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
                    log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] path [{}] successful", strategyName, storageId, repositoryId, backupPath);
                } catch (NoSuchFileException e) {
                    // 文件已删除，跳过处理
                    log.warn("Backup strategyName [{}] storageId [{}] repositoryId [{}] path [{}] no such file skip...", strategyName, storageId, repositoryId, file);
                    return FileVisitResult.CONTINUE;
                }
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private Path getTargetParentPath(String storageId, String repositoryId, String strategyName, String backupPath, boolean incremental) {
        Path targetParentPath = Paths.get(backupPath).resolve(strategyName);
        if (incremental) {
            //增量备份
            targetParentPath = targetParentPath.resolve("incremental");
        } else {
            //全量备份
            String time = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATETIME_PATTERN);
            targetParentPath = targetParentPath.resolve(time);
        }
        targetParentPath = targetParentPath.resolve(storageId).resolve(repositoryId);
        log.info("Backup strategyName [{}] storageId [{}] repositoryId [{}] target parent path [{}]", strategyName, storageId, repositoryId, targetParentPath);
        return targetParentPath;
    }


    private boolean ignoreDirectory(Repository repository, RepositoryPath repositoryPath) {
        String temp = ".temp";
        switch (repository.getLayout()) {
            case "Docker":
                return temp.equalsIgnoreCase(repositoryPath.getFileName().toString());
            default:
                return false;
        }
    }

    private boolean ignorePath(Repository repository, RepositoryPath repositoryPath) throws IOException {
        return false;
    }
}
