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

import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.enums.NpmRepositoryTypeEnum;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.services.NpmProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
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
public class NpmGroupProvider implements NpmProvider {

    @Inject
    private NpmProviderRegistry npmProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        npmProviderRegistry.addProvider(NpmRepositoryTypeEnum.NPM_GROUP.getType(), this);
        log.info("Registered npm provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), NpmRepositoryTypeEnum.NPM_GROUP.getType());
    }

    @Override
    public PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageVersion npmPackageVersionMetadata = null, subPubPackageVersionMetadata;
        npmPackageVersionMetadata = getLocalPackageVersion(repository, packageName, version, targetUrl);
        if (Objects.nonNull(npmPackageVersionMetadata)) {
            return npmPackageVersionMetadata;
        }
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subPubPackageVersionMetadata = npmProvider.packageVersion(subRepository, packageName, version, targetUrl);
                if (Objects.nonNull(subPubPackageVersionMetadata)) {
                    npmPackageVersionMetadata = subPubPackageVersionMetadata;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return npmPackageVersionMetadata;
    }

    @Override
    public PackageVersion getLocalPackageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageVersion npmPackageVersionMetadata = null, subPubPackageVersionMetadata;
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subPubPackageVersionMetadata = npmProvider.getLocalPackageVersion(subRepository, packageName, version, targetUrl);
                if (Objects.nonNull(subPubPackageVersionMetadata)) {
                    npmPackageVersionMetadata = subPubPackageVersionMetadata;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return npmPackageVersionMetadata;
    }

    @Override
    public PackageFeed packageFeed(Repository repository, String packageName, String targetUrl) {
        PackageFeed packageJson = null, subData;
        packageJson = getLocalPackageFeed(repository, packageName, targetUrl);
        if (Objects.nonNull(packageJson) && (
                (Objects.nonNull(packageJson.getVersions()) && packageJson.getVersions().getAdditionalProperties().size() > 0) || MapUtils.isNotEmpty(packageJson.getAdditionalProperties()))) {
            return packageJson;
        }
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = npmProvider.packageFeed(subRepository, packageName, targetUrl);
                if (Objects.nonNull(subData) && (
                        (Objects.nonNull(subData.getVersions()) && subData.getVersions().getAdditionalProperties().size() > 0) || MapUtils.isNotEmpty(subData.getAdditionalProperties()))) {
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
    public PackageFeed getLocalPackageFeed(Repository repository, String packageName, String targetUrl) {
        PackageFeed packageJson = null, subData;
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subData = npmProvider.getLocalPackageFeed(subRepository, packageName, targetUrl);
                if (Objects.nonNull(subData) && (
                        (Objects.nonNull(subData.getVersions()) && subData.getVersions().getAdditionalProperties().size() > 0) || MapUtils.isNotEmpty(subData.getAdditionalProperties()))) {
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
    public String binary(Repository repository, String packageName, String targetUrl) {
        String binaryData = null, subBinaryData;
        binaryData = getLocalBinary(repository, packageName, targetUrl);
        if (StringUtils.isNotBlank(binaryData)) {
            return binaryData;
        }
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subBinaryData = npmProvider.binary(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subBinaryData)) {
                    binaryData = subBinaryData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return binaryData;
    }

    @Override
    public String getLocalBinary(Repository repository, String packageName, String targetUrl) {
        String binaryData = null, subBinaryData;
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
                NpmProvider npmProvider = npmProviderRegistry.getProvider(NpmRepositoryTypeEnum.resolveType(subRepository.getType()));
                subBinaryData = npmProvider.getLocalBinary(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subBinaryData)) {
                    binaryData = subBinaryData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return binaryData;
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
