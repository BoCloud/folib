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
import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.domain.PubPackageVersionMetadata;
import com.folib.enums.PubRepositoryTypeEnum;
import com.folib.services.PubProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
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
public class PubGroupProvider implements PubProvider {

    @Inject
    private PubProviderRegistry pubProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        pubProviderRegistry.addProvider(PubRepositoryTypeEnum.PUB_GROUP.getType(), this);
        log.info("Registered pub provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PubRepositoryTypeEnum.PUB_GROUP.getType());
    }

    @Override
    public PubPackageVersionMetadata inspectVersion(Repository repository, String packageName, String version, String targetUrl) {
        PubPackageVersionMetadata pubPackageVersionMetadata = null, subPubPackageVersionMetadata;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                PubProvider pubProvider = pubProviderRegistry.getProvider(PubRepositoryTypeEnum.resolveType(subRepository.getType()));
                subPubPackageVersionMetadata = pubProvider.inspectVersion(subRepository, packageName, version, targetUrl);
                if (Objects.nonNull(subPubPackageVersionMetadata)) {
                    pubPackageVersionMetadata = subPubPackageVersionMetadata;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return pubPackageVersionMetadata;
    }

    @Override
    public JSONObject packages(Repository repository, String packageName, String targetUrl) {
        JSONObject packageJson = null, subData;
        packageJson = getLocalPackages(repository, packageName, targetUrl);
        if (Objects.nonNull(packageJson) && !JSONUtil.isNull(packageJson) && packageJson.keySet().size() > 0) {
            return packageJson;
        }
        PubPackageVersionMetadata pubPackageVersionMetadata = null, subPubPackageVersionMetadata;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                PubProvider pubProvider = pubProviderRegistry.getProvider(PubRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = pubProvider.packages(subRepository, packageName, targetUrl);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    packageJson = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return packageJson;
    }

    @Override
    public JSONObject getLocalPackages(Repository repository, String packageName, String targetUrl) {
        JSONObject packageJson = null, subData;
        PubPackageVersionMetadata pubPackageVersionMetadata = null, subPubPackageVersionMetadata;
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                Repository subRepository = configurationManager.getRepository(sId, rId);
                if (!isRepositoryResolvable(subRepository)) {
                    continue;
                }
                PubProvider pubProvider = pubProviderRegistry.getProvider(PubRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = pubProvider.getLocalPackages(subRepository, packageName, targetUrl);
                if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                    packageJson = subData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return packageJson;
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
