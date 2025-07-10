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
package com.folib.repository;

import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.locator.handlers.RemoveRawArtifactOperation;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * @author Veadan
 */
@Slf4j
@Component
public class RawRepositoryFeatures
        implements RepositoryFeatures {

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    public void removeRawArtifact(String storageId,
                                  String repositoryId,
                                  String artifactPath,
                                  int numberToKeep,
                                  Map<String, String> cleanupArtifactPathMap)
            throws IOException {
        Storage storage = getConfiguration().getStorage(storageId);
        if (Objects.isNull(storage)) {
            log.warn("Storage [{}] not found", storageId);
            return;
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            log.warn("Repository [{}] [{}] not found", storageId, repositoryId);
            return;
        }
        if (Boolean.FALSE.equals(repository.getEnableCustomLayout())) {
            log.warn("Repository [{}] [{}] custom layout not enabled", storageId, repositoryId);
            return;
        }
        if (StringUtils.isBlank(repository.getCustomLayout())) {
            log.warn("Repository [{}] [{}] custom layout not found", storageId, repositoryId);
            return;
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);

        RemoveRawArtifactOperation operation = new RemoveRawArtifactOperation();
        operation.setBasePath(repositoryPath);
        operation.setNumberToKeep(numberToKeep);
        operation.setCleanupArtifactPathMap(cleanupArtifactPathMap);
        operation.execute(repositoryPath);
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
