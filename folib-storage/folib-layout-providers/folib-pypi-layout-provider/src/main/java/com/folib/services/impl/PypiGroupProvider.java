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
import com.folib.enums.PypiRepositoryTypeEnum;
import com.folib.services.PypiProvider;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.List;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiGroupProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_GROUP.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.", getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_GROUP.getType());

    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String htmlData = null, subHtmlData;
        htmlData = getLocalPackages(repository, packageName, targetUrl);
        if (StringUtils.isNotBlank(htmlData)) {
            return htmlData;
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
                PypiProvider pypiProvider = pypiProviderRegistry.getProvider(PypiRepositoryTypeEnum.resolveType(subRepository.getType()));
                subHtmlData = pypiProvider.packages(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subHtmlData)) {
                    htmlData = subHtmlData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return htmlData;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        String htmlData = null, subHtmlData;
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
                PypiProvider pypiProvider = pypiProviderRegistry.getProvider(PypiRepositoryTypeEnum.resolveType(subRepository.getType()));
                subHtmlData = pypiProvider.getLocalPackages(subRepository, packageName, targetUrl);
                if (StringUtils.isNotBlank(subHtmlData)) {
                    htmlData = subHtmlData;
                    break;
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return htmlData;
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
