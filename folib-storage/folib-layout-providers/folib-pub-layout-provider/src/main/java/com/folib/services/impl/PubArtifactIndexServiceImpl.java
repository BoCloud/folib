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
package com.folib.services.impl;

import com.folib.components.DistributedLockComponent;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.enums.PubIndexTypeEnum;
import com.folib.indexer.PubPackageMetadataIndexer;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.PubLayoutProvider;
import com.folib.services.PubArtifactIndexService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.utils.PubUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Service
public class PubArtifactIndexServiceImpl implements PubArtifactIndexService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Inject
    private PubPackageMetadataIndexer pubPackageMetadataIndexer;

    @Override
    public void rebuildIndex(String storageId, String repositoryId, String artifactPath) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (!PubLayoutProvider.ALIAS.equals(repository.getLayout())) {
            log.warn("Trying to rebuild index of repository {} with unsupported layout {} ", repository.getId(),
                    repository.getLayout());
            return;
        }

        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(repository);
        if (artifactPath != null && artifactPath.trim().length() > 0) {
            repositoryBasePath = repositoryBasePath.resolve(artifactPath);
        }
        if (!Files.exists(repositoryBasePath)) {
            return;
        }
        String key = String.format("PubMetadata_%s_%s", storageId, repositoryId);
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME * GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                try (Stream<Path> pathStream = Files.list(repositoryBasePath)) {
                    pathStream.filter(Files::isDirectory)
                            // Skip directories which start with a dot (like, for example: .index)
                            .filter(this::isArtifactDirectory)
                            // Note: Sorting can be expensive:
                            .sorted()
                            .forEach(this::execute);
                } catch (IOException ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    @Override
    public void rebuildIndex(RepositoryPath repositoryPath) {
        try {
            rebuildIndex(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    public boolean isArtifactDirectory(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath)|| RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length == 1 && PubUtils.isNameFieldValid(relativizePath)) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    private void execute(Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            pubPackageMetadataIndexer.indexAsSystem(repositoryPath, PubIndexTypeEnum.REINDEX);
        } catch (Exception ex) {
            log.error("Rebuild index error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }
}
