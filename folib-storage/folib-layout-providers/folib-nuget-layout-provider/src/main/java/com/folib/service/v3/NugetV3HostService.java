package com.folib.service.v3;

import com.folib.cache.NugetPackageCache;
import com.folib.cache.NugetSearchCountCache;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.NugetSemVerUtil;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.nugetv3.registration.NugetV3RegistrationUtil;
import com.folib.nugetv3.search.NugetSearchResultUtil;
import com.folib.nugetv3.search.NugetV3HostSearchUtil;
import com.folib.service.NugetV3Service;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;



@Component
@Slf4j
public class NugetV3HostService implements NugetV3Service {
    private final NugetV3HostSearchUtil nugetHostSearchUtil;

    private final NugetPackageCache nugetPackageCache;

    private final NugetSearchCountCache nugetSearchCountCache;

    @Autowired
    public NugetV3HostService(NugetV3HostSearchUtil nugetHostSearchUtil, NugetPackageCache nugetPackageCache, NugetSearchCountCache nugetSearchCountCache) {
        this.nugetHostSearchUtil = nugetHostSearchUtil;
        this.nugetPackageCache = nugetPackageCache;
        this.nugetSearchCountCache = nugetSearchCountCache;
    }

    @Override
    public String getType() {
        return "HOST";
    }

    @Override
    public RegistrationResult getRegistration(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        List<RegistrationResultPageItem> registrationResultPageItems = this.getRegistrationPageItems(repository, packageId, isSemVer2Endpoint);
        if (registrationResultPageItems == null || registrationResultPageItems.isEmpty()) {
            return null;
        }
        String v3RegistrationBaseUrl =  NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(repository, isSemVer2Endpoint);
        return NugetV3RegistrationUtil.registrationResultPageItemsToRegistrationResult(registrationResultPageItems, v3RegistrationBaseUrl);
    }

    @Override
    public RegistrationResultPage getRegistrationPage(Repository repository, String packageId, String lower, String upper, boolean isSemVer2Endpoint) throws Exception {
        List<RegistrationResultPageItem> registrationResultPageItems = this.getRegistrationPageItems(repository, packageId, isSemVer2Endpoint);
        if (registrationResultPageItems == null || registrationResultPageItems.isEmpty()) {
            return null;
        }
        String v3RegistrationBaseUrl =  NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(repository, isSemVer2Endpoint);
        return NugetV3RegistrationUtil.registrationResultPageItemsToRegistrationResultPage(registrationResultPageItems, packageId, lower, upper, v3RegistrationBaseUrl);
    }

    @Override
    public RegistrationResultPageItem getRegistrationPageItem(Repository repository, String packageId, String version) throws Exception {
        List<RegistrationResultPageItem> registrationResultPageItems = this.getRegistrationPageItems(repository, packageId, true);
        if (registrationResultPageItems == null || registrationResultPageItems.isEmpty()) {
            return null;
        }
        for (RegistrationResultPageItem item : registrationResultPageItems) {
            if (item.getCatalogEntry().getVersion().equalsIgnoreCase(version)) {
                return item;
            }
        }
        return null;
    }

    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {
            if (nugetSearchCountCache.containsV3(searchRequest)) {
                return nugetSearchCountCache.getV3(searchRequest);
            }
            SearchResult searchResult = this.search(searchRequest);
            int count = searchResult.getTotalHits();
            nugetSearchCountCache.putV3(searchRequest, count);
            return count;
        } catch (Exception e) {
            log.error("Error occurred while counting search results: ", e);
            return 0;
        }
    }

    @Override
    public SearchResult search(NugetSearchRequest searchRequest) throws Exception {
        Map<String, Map<String, RegistrationResultPageItem>> allPackages = nugetHostSearchUtil.searchHostItems(searchRequest);
        boolean isSemver2 = searchRequest.getShouldRemoveSemver2() == null || !searchRequest.getShouldRemoveSemver2();
        String v3RegistrationBaseUrl = NuGetUrlBuilder.getNugetV3RegistrationBaseUrl(searchRequest.getRepository(), isSemver2);
        SearchResult searchResult = NugetSearchResultUtil.generateSearchResult(allPackages, v3RegistrationBaseUrl);
        // 进行分页处理
        searchResult = NugetSearchResultUtil.page(searchResult, searchRequest.getSkip(), searchRequest.getTake());
        return searchResult;
    }

    private List<RegistrationResultPageItem> getRegistrationPageItems(Repository repository, String packageId, boolean isSemVer2Endpoint) {
        String lower_id = packageId.toLowerCase();
        List<RegistrationResultPageItem> items = nugetPackageCache.getRegistrationResultPageItem(repository, lower_id);
        if (items != null && !isSemVer2Endpoint) {
            // 去除SemVer2的版本
            items.removeIf(item -> NugetSemVerUtil.isSemVerLevel2(item.getCatalogEntry().getVersion()));
        }
        return items;
    }
}
