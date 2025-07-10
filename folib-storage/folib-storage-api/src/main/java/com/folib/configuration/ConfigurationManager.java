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
package com.folib.configuration;

import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.net.URI;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 */
@Slf4j
@Component
public class ConfigurationManager implements StoragesConfigurationManager {

    @Inject
    @Lazy
    private ConfigurationManagementService configurationService;

    public Repository getRepository(String storageAndRepositoryId) {
        String[] elements = storageAndRepositoryId.split(":");
        String storageId = elements[0];
        String repositoryId = elements[1];

        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Repository getRepository(String storageId,
                                    String repositoryId) {
        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Storage getStorage(String storageId) {
        return getConfiguration().getStorage(storageId);
    }

    public Configuration getConfiguration() {
        return configurationService.getConfiguration();
    }

    public URI getBaseUri() {
        try {
            return URI.create(getConfiguration().getBaseUrl());
        } catch (IllegalArgumentException e) {
            throw new InvalidConfigurationException(e);
        }
    }

    public Integer getSessionTimeoutSeconds() {
        return getConfiguration().getSessionConfiguration().getTimeoutSeconds();
    }

    public List<String> resolveGroupRepository(Repository repository, List<String> storageAndRepositoryIdList) {
        if (CollectionUtils.isNotEmpty(repository.getGroupRepositories())) {
            String rootStorageAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(repository.getStorage().getId(), repository.getId());
            for (String storageAndRepositoryId : repository.getGroupRepositories()) {
                if (storageAndRepositoryId.equalsIgnoreCase(rootStorageAndRepositoryId)) {
                    continue;
                }
                String sId = ConfigurationUtils.getStorageId(storageAndRepositoryId, storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                try {
                    Storage storage = getConfiguration().getStorage(sId);
                    if (Objects.nonNull(storage)) {
                        Repository subRepository = storage.getRepository(rId);
                        if (Objects.nonNull(subRepository)) {
                            if (!isRepositoryResolvable(subRepository)) {
                                continue;
                            }
                            if (RepositoryTypeEnum.GROUP.getType().equals(subRepository.getType())) {
                                resolveGroupRepository(subRepository, storageAndRepositoryIdList);
                            } else if (!storageAndRepositoryIdList.contains(storageAndRepositoryId)) {
                                storageAndRepositoryIdList.add(storageAndRepositoryId);
                            }
                        }
                    }
                } catch (Exception ex) {
                    log.error("group repository resolvePathTraversal storageId: [{}] repositoryId [{}] error：[{}]", sId, rId, ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        log.info("Repository [{}] [{}] storageAndRepositoryIdList [{}]", repository.getStorage().getId(), repository.getId(), String.join(",", storageAndRepositoryIdList));
        return storageAndRepositoryIdList;
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }
}
