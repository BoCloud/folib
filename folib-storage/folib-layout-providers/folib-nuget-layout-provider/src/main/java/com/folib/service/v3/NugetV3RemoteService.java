package com.folib.service.v3;

import com.folib.cache.NugetRemoteUrlCache;
import com.folib.cache.NugetSearchCountCache;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.utils.proxy.FetchType;
import com.folib.nuget.utils.proxy.NugetRemoteUtil;
import com.folib.nuget.utils.proxy.NugetRemoteV3UrlBuilder;
import com.folib.nugetv3.model.dependency.Dependency;
import com.folib.nugetv3.model.dependency.DependencyGroupsItem;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultCatalogEntry;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.nugetv3.model.search.SearchResultData;
import com.folib.nugetv3.model.search.SearchResultDataVersion;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.NugetV3Service;
import com.folib.storage.repository.Repository;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;



@Component
@Slf4j
public class NugetV3RemoteService implements NugetV3Service {
    private final RepositoryPathResolver repositoryPathResolver;

    private final NugetRemoteUrlCache nugetRemoteUrlCache;

    private final NugetSearchCountCache nugetSearchCountCache;

    @Autowired
    public NugetV3RemoteService(RepositoryPathResolver repositoryPathResolver, NugetRemoteUrlCache nugetRemoteRegistrationItemUrlCache, NugetSearchCountCache nugetSearchCountCache) {
        this.repositoryPathResolver = repositoryPathResolver;
        this.nugetRemoteUrlCache = nugetRemoteRegistrationItemUrlCache;
        this.nugetSearchCountCache = nugetSearchCountCache;
    }


    // 需缓存 /index.json
//    public NugetServiceIndex getNugetProxyServiceIndex(Repository repository) throws Exception {
//        return NugetRemoteV3UrlBuilder.getNugetProxyServiceIndex(repository);
//    }


    @Override
    public String getType() {
        return "PROXY";
    }

    // 需缓存 /registration/{packageId}.json and /registration-semver2/{packageId}.json
    @Override
    public RegistrationResult getRegistration(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        String subPath = isSemVer2Endpoint ? "registration-semver2/" + packageId + ".json" : "registration/" + packageId + ".json";
        RepositoryPath registrationPath = repositoryPathResolver.resolve(repository, subPath);
        String remoteUrl = NugetRemoteV3UrlBuilder.getRemoteRegistrationUrl(repository, packageId, isSemVer2Endpoint);
        RegistrationResult result = NugetRemoteUtil.fetchAndCache(remoteUrl, RegistrationResult.class, registrationPath, FetchType.JSON);
        // 缓存结果
        cacheRegistrationResult(repository, packageId, isSemVer2Endpoint, result);
        return result;
    }

    @Override
    public RegistrationResultPage getRegistrationPage(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) throws Exception {
        packageId = packageId.toLowerCase();
        // 1. 远端地址
        String remoteUrl = NugetRemoteV3UrlBuilder.getRemoteRegistrationPageUrl(repository, packageId, lower, upper, isSemVer2Endpoint);
        // 2. 第一次拉取
        RegistrationResultPage page = NugetRemoteUtil.fetchFromRemote(remoteUrl, RegistrationResultPage.class, FetchType.JSON);
        if (page.getItems() != null) {
            // 缓存
            cacheRegistrationResultPage(repository, packageId, isSemVer2Endpoint, page);
            return page;
        }
        // 没有items, 则是简化版, 应拉取其id(https://learn.microsoft.com/zh-cn/nuget/api/registration-base-url-resource)
        // 3. 再次拉取
        page = NugetRemoteUtil.fetchFromRemote(page.getId(), RegistrationResultPage.class, FetchType.JSON);
        // 缓存
        cacheRegistrationResultPage(repository, packageId, isSemVer2Endpoint, page);
        return page;
    }


    // 需缓存 /{packageId}/{version}/index.json
    @Override
    public RegistrationResultPageItem getRegistrationPageItem(Repository repository, String packageId, String version) throws Exception {
        // 1. 构造本地存储路径
        packageId = packageId.toLowerCase();
        String subPath = String.format("%s/%s/index.json", packageId, version);
        RepositoryPath registrationPath = repositoryPathResolver.resolve(repository, subPath);

        // 2. 获取远程地址
        String remoteUrl = nugetRemoteUrlCache.getItem(repository, packageId, version);

        // 3. 从远程获取并缓存
        return NugetRemoteUtil.fetchAndCache(remoteUrl, RegistrationResultPageItem.class, registrationPath, FetchType.JSON);
    }

    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {
            if (nugetSearchCountCache.containsV3(searchRequest)) {
                return nugetSearchCountCache.getV3(searchRequest);
            }
            SearchResult searchResult = search(searchRequest);
            int count = searchResult.getTotalHits();
            nugetSearchCountCache.putV3(searchRequest, count);
            return count;
        } catch (Exception e) {
            log.error("Error while searching count for request: {}", searchRequest, e);
            return 0;
        }
    }


    @Override
    public SearchResult search(NugetSearchRequest searchRequest) throws Exception{
        Repository repository = searchRequest.getRepository();
        String remoteUrl = NugetRemoteV3UrlBuilder.getRemoteSearchUrl(repository, searchRequest);
        SearchResult result = NugetRemoteUtil.fetchFromRemote(remoteUrl, SearchResult.class, FetchType.JSON);
        cacheSearchResult(repository, result);
        return result;
    }

    // 缓存RegistrationResult: pageUrl and itemUrl
    private void cacheRegistrationResult(@NonNull Repository repository, @NonNull String packageId, boolean isSemVer2Endpoint, @NonNull RegistrationResult registrationResult) throws Exception {
        for (RegistrationResultPage page : registrationResult.getItems()) {
            cacheRegistrationResultPage(repository, packageId, isSemVer2Endpoint, page);
        }
    }

    // 缓存RegistrationResultPage: itemUrl
    private void cacheRegistrationResultPage(@NonNull Repository repository, @NonNull String packageId, boolean isSemVer2EndPoint, @NonNull RegistrationResultPage page) throws Exception {
        if (page.getItems() == null) {
            return;
        }
        // 缓存pageUrl
        String pageUrl = page.getId();
        String lower = page.getLower() != null ? page.getLower() : "0.0.0";
        String upper = page.getUpper() != null ? page.getUpper() : "9999.9999.9999";
        nugetRemoteUrlCache.putPage(repository, packageId, lower, upper, pageUrl, isSemVer2EndPoint);
        for (RegistrationResultPageItem item : page.getItems()) {
            cacheRegistrationResultPageItem(repository, item);
        }
    }

    private void cacheRegistrationResultPageItem(@NonNull Repository repository, @NonNull RegistrationResultPageItem item) {
        RegistrationResultCatalogEntry entry = item.getCatalogEntry();
        String packageId = entry.getPackageId();
        String version = entry.getVersion();
        // 1. 缓存content
        String content = item.getPackageContent();
        nugetRemoteUrlCache.putContent(repository, packageId, version, content);
        // 2. 缓存registration-item
        String itemUrl = item.getId();
        nugetRemoteUrlCache.putItem(repository, packageId, version, itemUrl);
        // 3. 缓存依赖
        List<DependencyGroupsItem> dependencyGroups = entry.getDependencyGroups();
        if (dependencyGroups != null) {
            for (DependencyGroupsItem dependencyGroupsItem : dependencyGroups) {
                cacheRegistrationDependencyGroupItem(repository, dependencyGroupsItem);
            }
        }
    }

    private void cacheRegistrationDependencyGroupItem(@NonNull Repository repository, @NonNull DependencyGroupsItem dependencyGroupsItem) {
        if (dependencyGroupsItem.getDependencies() == null) {
            return;
        }
        for (Dependency dependency : dependencyGroupsItem.getDependencies()) {
            String packageId = dependency.getPackageId();
            String registrationUrl = dependency.getRegistration();
            nugetRemoteUrlCache.putRegistration(repository, packageId, registrationUrl);
        }
    }

    private void cacheSearchResult(Repository repository, SearchResult result) {
        List<SearchResultData> dataList = result.getData();
        if (dataList == null || dataList.isEmpty()) {
            return;
        }
        for (SearchResultData data : dataList) {
            cacheSearchResultData(repository, data);
        }
    }

    public void  cacheSearchResultData(Repository repository, SearchResultData data) {
        if (data == null) {
            return;
        }
        String packageId = data.getPackageId();
        String version = data.getVersion();
        String itemUrl = data.getRegistration();
        nugetRemoteUrlCache.putItem(repository, packageId, version, itemUrl);
        List<SearchResultDataVersion> versions = data.getVersions();
        if (versions == null || versions.isEmpty()) {
            return;
        }
        for (SearchResultDataVersion versionData : versions) {
            version = versionData.getVersion();
            String versionUrl = versionData.getId();
            nugetRemoteUrlCache.putItem(repository, packageId, version, versionUrl);
        }
    }
}
