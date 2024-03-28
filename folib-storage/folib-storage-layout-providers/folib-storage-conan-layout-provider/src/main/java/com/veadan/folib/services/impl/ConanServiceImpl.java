package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.services.ConanProvider;
import com.veadan.folib.services.ConanService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author leipenghui
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
