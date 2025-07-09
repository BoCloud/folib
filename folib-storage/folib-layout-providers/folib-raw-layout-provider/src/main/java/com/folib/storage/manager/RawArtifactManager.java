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
package com.folib.storage.manager;

import com.folib.constant.GlobalConstants;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 */
@Component
public class RawArtifactManager {


    private static final Logger logger = LoggerFactory.getLogger(RawArtifactManager.class);


    public RawArtifactManager() {
    }

    public void deleteArtifacts(RepositoryPath basePath, LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths, int numberToKeep, Map<String, String> cleanupArtifactPathMap)
            throws IOException {
        Repository repository = basePath.getRepository();
        if (!Files.exists(basePath)) {
            logger.warn("Removal of Raw artifact: {} not exists.", basePath);
            return;
        }
        logger.info("Removal of Raw artifact {} in '{}:{}'.",
                basePath, repository.getStorage().getId(), repository.getId());
        for (Map.Entry<RepositoryPath, List<RepositoryPath>> entry : visitedRootPaths.entrySet()) {
            RepositoryPath repositoryPath = entry.getKey();
            try {
                List<RepositoryPath> repositoryPathList = entry.getValue();
                int size = repositoryPathList.size();
                int artifactNumberToKeep = getNumberToKeep(RepositoryFiles.relativizePath(repositoryPath), numberToKeep, cleanupArtifactPathMap);
                logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] version count [{}] numberToKeep [{}] versions [{}]", basePath.getStorageId(), basePath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath), size, artifactNumberToKeep, repositoryPathList);
                if (artifactNumberToKeep > 0 && size > 1 && size > artifactNumberToKeep) {
                    List<RepositoryPath> versions = repositoryPathList.subList(0, size - artifactNumberToKeep);
                    logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] version count [{}] numberToKeep [{}] delete versions [{}]", basePath.getStorageId(), basePath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath), size, artifactNumberToKeep, versions);
                    for (RepositoryPath deleteRepositoryPath : versions) {
                        handlerRepositoryPath(deleteRepositoryPath, repository.isAllowsForceDeletion());
                    }
                }
            } catch (Exception ex) {
                logger.error("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] error [{}]", basePath.getStorageId(), basePath.getRepositoryId(), repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private void handlerRepositoryPath(RepositoryPath deleteRepositoryPath, boolean force) {
        try {
            if (!Files.exists(deleteRepositoryPath)) {
                return;
            }
            RepositoryFiles.delete(deleteRepositoryPath, force);
            logger.info("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] force [{}]", deleteRepositoryPath.getStorageId(), deleteRepositoryPath.getRepositoryId(), deleteRepositoryPath, force);
        } catch (Exception ex) {
            logger.error("Remove raw artifact job storageId [{}] repositoryId [{}] path [{}] force [{}] error [{}]", deleteRepositoryPath.getStorageId(), deleteRepositoryPath.getRepositoryId(), deleteRepositoryPath, force, ExceptionUtils.getStackTrace(ex));
        }
    }

    private int getNumberToKeep(String artifactPath, int numberToKeep, Map<String, String> cleanupArtifactPathMap) {
        if (MapUtils.isEmpty(cleanupArtifactPathMap)) {
            return numberToKeep;
        }
        String cleanupArtifactPath, cleanupArtifactPathValue, cleanupArtifactPathPrefix;
        for (Map.Entry<String, String> entry : cleanupArtifactPathMap.entrySet()) {
            cleanupArtifactPath = entry.getKey();
            cleanupArtifactPathValue = entry.getValue();
            if (StringUtils.isBlank(cleanupArtifactPath) || StringUtils.isBlank(cleanupArtifactPathValue)) {
                continue;
            }
            //获取目录、制品级别生命周期，优先级第一
            cleanupArtifactPathPrefix = cleanupArtifactPath + GlobalConstants.SEPARATOR;
            if (artifactPath.equals(cleanupArtifactPath) || artifactPath.startsWith(cleanupArtifactPathPrefix) || artifactPath.matches(cleanupArtifactPath)) {
                return Integer.parseInt(entry.getValue());
            }
        }
        //仓库级别生命周期，优先级最低
        return numberToKeep;
    }
}
