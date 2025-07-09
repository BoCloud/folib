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

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.domain.SearchResults;
import com.folib.enums.ConanSearchRepositoryTypeEnum;
import com.folib.services.ConanProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.List;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ConanGroupProvider implements ConanProvider {

    @Inject
    private ConanProviderRegistry conanProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        conanProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_GROUP.getType(), this);
        log.info("Registered conan provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_GROUP.getType());
    }

    @Override
    public SearchResults search(String version, Repository repository, String query) {
        SearchResults searchResults = SearchResults.builder().results(Lists.newArrayList()).build(), subSearchResults;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subSearchResults = conanSearchProvider.search(version, subRepository, query);
                if (Objects.nonNull(subSearchResults) && CollectionUtils.isNotEmpty(subSearchResults.getResults())) {
                    searchResults.getResults().addAll(subSearchResults.getResults());
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return searchResults;
    }

    @Override
    public JSONObject revisionsSearch(Repository repository, String artifactPath, String url) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.revisionsSearch(subRepository, artifactPath, url);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject revisions(Repository repository, String artifactPath, String targetUrl) {
        JSONObject data = null, subData;
        data = getLocalRevisions(repository, artifactPath, targetUrl);
        if (Objects.nonNull(data) && !JSONUtil.isNull(data) && data.keySet().size() > 0) {
            return data;
        }
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.revisions(subRepository, artifactPath, targetUrl);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject getLocalRevisions(Repository repository, String artifactPath, String targetUrl) {
        JSONObject data = null, subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.getLocalRevisions(subRepository, artifactPath, targetUrl);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject downloadUrls(Repository repository, String name, String version, String user, String channel) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.downloadUrls(subRepository, name, version, user, channel);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject packageDownloadUrls(Repository repository, String name, String version, String user, String channel, String packageId) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.packageDownloadUrls(subRepository, name, version, user, channel, packageId);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject digest(Repository repository, String name, String version, String user, String channel) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.digest(subRepository, name, version, user, channel);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject packageDigest(Repository repository, String name, String version, String user, String channel, String packageId) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.packageDigest(subRepository, name, version, user, channel, packageId);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
    }

    @Override
    public JSONObject getPackageInfo(Repository repository, String name, String version, String user, String channel, String packageId, String url) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : getStorageAndRepositoryIdList(repository)) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = conanSearchProvider.getPackageInfo(subRepository, name, version, user, channel, packageId, url);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    data = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return data;
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

    private List<String> getStorageAndRepositoryIdList(Repository repository) {
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        return configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
    }

}
