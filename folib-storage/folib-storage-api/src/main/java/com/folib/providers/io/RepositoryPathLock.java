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
package com.folib.providers.io;

import com.hazelcast.core.HazelcastInstance;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ProductTypeEnum;
import com.folib.util.UriUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import javax.annotation.Nonnull;
import jakarta.inject.Inject;

import java.io.IOException;
import java.net.URI;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 */
@Component
public class RepositoryPathLock {

    private static final Logger logger = LoggerFactory.getLogger(RepositoryPathLock.class);

    @Inject
    private HazelcastInstance hazelcastInstance;

    public boolean lock(final @Nonnull RepositoryPath repositoryPath) throws IOException {
        return lock(repositoryPath, null);
    }

    public boolean lock(final @Nonnull RepositoryPath repositoryPath,
                        String id) throws IOException {
        String lockName = id;
        if (StringUtils.isBlank(id)) {
            lockName = getLockName(repositoryPath);
        }
        logger.debug("Get lock for [{}]", lockName);
        try {
            return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).tryLock(lockName, 1200L, TimeUnit.SECONDS, 1800, TimeUnit.SECONDS);
        } catch (Exception ex) {
            logger.warn(ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }

    public void unLock(String lockName) {
        if (hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).isLocked(lockName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).forceUnlock(lockName);
            logger.debug("Unlocked for [{}]", lockName);
        } else {
            logger.debug("LockName for [{}] not locked", lockName);
        }
    }

    public void unLock(RepositoryPath repositoryPath) {
        try {
            final String lockName = getLockName(repositoryPath);
            unLock(lockName);
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    private String getLockName(RepositoryPath repositoryPath) throws IOException {
        URI uri = getURI(repositoryPath);
        String lockName = uri.toString();
        if (ProductTypeEnum.Docker.getFoLibraryName().equals(repositoryPath.getRepository().getLayout())) {
            //docker布局
            final String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST.stream().anyMatch(artifactPath::startsWith)) {
                lockName = artifactPath;
            }
        }
        return String.format("%s:%s:%s", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), lockName);
    }

    public int getLockInfo() {
        return hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_LOCK_NAME).size();
    }

    private URI getURI(final @Nonnull RepositoryPath repositoryPath) throws IOException {
        if (RepositoryFiles.isArtifact(repositoryPath)) {
            ArtifactCoordinates c = RepositoryFiles.readCoordinates(repositoryPath);
            return URI.create(UriUtils.encode(c.getId()));
        }
        final URI uri = repositoryPath.toUri();
        Assert.isTrue(uri.isAbsolute(), String.format("Unable to lock relative path %s", uri));
        return uri;
    }

}
