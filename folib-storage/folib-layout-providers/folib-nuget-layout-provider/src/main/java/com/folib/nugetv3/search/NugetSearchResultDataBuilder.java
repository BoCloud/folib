package com.folib.nugetv3.search;

import com.folib.nuget.utils.NugetVersionUtil;
import com.folib.nugetv3.model.registration.RegistrationResultCatalogEntry;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResultData;
import com.folib.nugetv3.model.search.SearchResultDataVersion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;




public class NugetSearchResultDataBuilder {

    public static SearchResultData buildSearchResultData(Map<String, RegistrationResultPageItem> packageVersionsProps, String packageId, String v3RegistrationPackageUrl) {
        // 1. 获取最新版本
        RegistrationResultPageItem latestVersionItem = getLatestVersionItem(packageVersionsProps);
        RegistrationResultCatalogEntry catalogEntry = latestVersionItem.getCatalogEntry();

        // 2. 获取所有版本
        List<SearchResultDataVersion> versions = buildVersions(packageVersionsProps);

        // 3. 组织数据
        SearchResultData searchResultData = new SearchResultData();
        searchResultData.setId(v3RegistrationPackageUrl);
        searchResultData.setRegistration(v3RegistrationPackageUrl);
        searchResultData.setPackageId(packageId);
        searchResultData.setVersion(catalogEntry.getVersion());
        searchResultData.setDescription(catalogEntry.getDescription());
        searchResultData.setSummary(catalogEntry.getSummary());
        searchResultData.setTitle(catalogEntry.getTitle());
        searchResultData.setIconUrl(catalogEntry.getIconUrl());
        searchResultData.setLicenseUrl(catalogEntry.getLicenseUrl());
        searchResultData.setProjectUrl(catalogEntry.getProjectUrl());
        searchResultData.setTags(catalogEntry.getTags());
        ArrayList <String> authors = new ArrayList<>();
        authors.add(catalogEntry.getAuthors());
        searchResultData.setAuthors(authors);
        searchResultData.setTotalDownloads(0);
        searchResultData.setVerified(false);
        searchResultData.setVersions(versions);

        return searchResultData;
    }

    private static RegistrationResultPageItem getLatestVersionItem(Map<String, RegistrationResultPageItem> packageVersionsProps) {
        RegistrationResultPageItem latestVersionItem = null;
        String latestVersion = null;
        for (Map.Entry<String, RegistrationResultPageItem> entry : packageVersionsProps.entrySet()) {
            String version = entry.getKey();
            RegistrationResultPageItem item = entry.getValue();
            if (latestVersion == null || NugetVersionUtil.lessOrEqual(latestVersion, version)) {
                latestVersion = version;
                latestVersionItem = item;
            }
        }
        return latestVersionItem;
    }

    private static List<SearchResultDataVersion> buildVersions(Map<String, RegistrationResultPageItem> packageVersionsProps) {
        List<SearchResultDataVersion> versions = new ArrayList<>();
        for (Map.Entry<String, RegistrationResultPageItem> entry : packageVersionsProps.entrySet()) {
            RegistrationResultPageItem item = entry.getValue();
            RegistrationResultCatalogEntry catalogEntry = item.getCatalogEntry();
            SearchResultDataVersion versionItem = SearchResultDataVersion.builder()
                    .id(item.getId())
                    .version(catalogEntry.getVersion())
                    .downloads(0)
                    .build();
            versions.add(versionItem);
        }
        return versions;
    }
}
