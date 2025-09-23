package com.folib.nugetv3.search;

import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.nugetv3.model.search.SearchResult;
import com.folib.nugetv3.model.search.SearchResultData;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;



public class NugetSearchResultUtil {

    public static SearchResult mergeSearchResults(List<SearchResult> searchResults) {
        SearchResult mergedResult = new SearchResult();
        for (SearchResult searchResult : searchResults) {
            if (searchResult != null && searchResult.getData() != null) {
                mergedResult.merge(searchResult);
            }
        }
        return mergedResult;
    }

    public static SearchResult page(SearchResult searchResult, Integer begin, Integer size) {
        if (searchResult == null || searchResult.getData() == null || searchResult.getData().isEmpty()) {
            return new SearchResult();
        }
        List<SearchResultData> data = searchResult.getData();
        int totalSize = data.size();
        if (begin == null || begin < 0) {
            begin = 0;
        }
        if (begin >= totalSize) {
            return new SearchResult();
        }
        if (size == null || size <= 0) {
            size = totalSize - begin; // 如果size无效，则返回剩余所有数据
        }
        int end = Math.min(begin + size, totalSize);

        List<SearchResultData> pagedData = data.subList(begin, end);
        return new SearchResult(pagedData);

    }


    public static SearchResult generateSearchResult(Map<String, Map<String, RegistrationResultPageItem>> packages, String v3RegistrationBaseUrl) {
        List<SearchResultData> searchResults = new ArrayList<>();
        for (Map.Entry<String, Map<String, RegistrationResultPageItem>> entry : packages.entrySet()) {
            String packageId = entry.getKey();
            Map<String, RegistrationResultPageItem> packageVersionsProps = entry.getValue();

            // 1. 获取最新版本的URL
            String v3RegistrationPackageUrl = NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId);

            // 2. 生成SearchResultData
            SearchResultData searchResultData = NugetSearchResultDataBuilder.buildSearchResultData(packageVersionsProps, packageId, v3RegistrationPackageUrl);
            searchResults.add(searchResultData);
        }
        return new SearchResult(searchResults);
    }
}
