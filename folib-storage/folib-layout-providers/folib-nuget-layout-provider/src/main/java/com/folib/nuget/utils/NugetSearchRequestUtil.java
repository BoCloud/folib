package com.folib.nuget.utils;

import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.storage.repository.Repository;




public class NugetSearchRequestUtil {
    public static NugetSearchRequest subSearchRequestWithoutPage(NugetSearchRequest searchRequest, Repository subRepo) {
        return NugetSearchRequest.builder()
                .repository(subRepo)
                .searchTerm(searchRequest.getSearchTerm())
                .includePreRelease(searchRequest.includePreRelease())
                .shouldRemoveSemver2(searchRequest.getShouldRemoveSemver2())
                .build();
    }

    public static NugetSearchRequest subSearchRequest(NugetSearchRequest searchRequest, Repository subRepo, int skip, int take) {
        return NugetSearchRequest.builder()
                .repository(subRepo)
                .searchTerm(searchRequest.getSearchTerm())
                .includePreRelease(searchRequest.includePreRelease())
                .shouldRemoveSemver2(searchRequest.getShouldRemoveSemver2())
                .skip(skip)
                .take(take)
                .build();
    }
}
