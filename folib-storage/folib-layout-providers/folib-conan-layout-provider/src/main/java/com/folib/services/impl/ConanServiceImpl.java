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

import com.alibaba.fastjson.JSONObject;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.SearchResults;
import com.folib.enums.ConanSearchRepositoryTypeEnum;
import com.folib.services.ConanProvider;
import com.folib.services.ConanService;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author veadan
 * @date 2024/3/25
 **/
@Slf4j
@Service
public class ConanServiceImpl implements ConanService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ConanProviderRegistry conanProviderRegistry;

    @Override
    public SearchResults search(String version, Repository repository, String query) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.search(version, repository, query);
    }

    @Override
    public Object revisionsSearch(Repository repository, String artifactPath, String url) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.revisionsSearch(repository, artifactPath, url);
    }

    @Override
    public JSONObject revisions(Repository repository, String artifactPath, String targetUrl) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.revisions(repository, artifactPath, targetUrl);
    }

    @Override
    public JSONObject downloadUrls(Repository repository, String name, String version, String user, String channel) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.downloadUrls(repository, name, version, user, channel);
    }

    @Override
    public JSONObject packageDownloadUrls(Repository repository, String name, String version, String user, String channel, String packageId) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.packageDownloadUrls(repository, name, version, user, channel, packageId);
    }

    @Override
    public JSONObject digest(Repository repository, String name, String version, String user, String channel) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.digest(repository, name, version, user, channel);
    }

    @Override
    public JSONObject packageDigest(Repository repository, String name, String version, String user, String channel, String packageId) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.packageDigest(repository, name, version, user, channel, packageId);
    }

    @Override
    public JSONObject getPackageInfo(Repository repository, String name, String version, String user, String channel, String packageId, String url) {
        ConanProvider conanSearchProvider = conanProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.getPackageInfo(repository, name, version, user, channel, packageId, url);
    }
}
