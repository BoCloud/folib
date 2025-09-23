package com.folib.nuget.filter;

import com.folib.nuget.utils.NugetSemVerUtil;
import lombok.AllArgsConstructor;
import org.apache.commons.lang.StringUtils;


@AllArgsConstructor
public class NuGetSearchFilter {
    private final String searchTerm;
    private final boolean includePreRelease;
    private boolean shouldRemoveSemVer2Results;

    public NuGetSearchFilter(NugetSearchRequest searchRequest) {
        this.searchTerm = searchRequest.getSearchTerm();
        this.includePreRelease = searchRequest.includePreRelease();
        this.shouldRemoveSemVer2Results = searchRequest.shouldRemoveSemVer2Results();
    }

    public boolean accepts(String id, String version, String description) {
        if (!this.includePreRelease && StringUtils.contains(version, "-")) {
            return false;
        }
        if (this.shouldRemoveSemVer2Results && NugetSemVerUtil.isSemVerLevel2(version)) {
            return false;
        }
        if (searchTerm == null || StringUtils.isBlank(this.searchTerm)) {
            return true; // If search term is empty, accept all
        }
        boolean first = StringUtils.containsIgnoreCase(id, this.searchTerm);
        boolean second = StringUtils.containsIgnoreCase(description, this.searchTerm);
        return (StringUtils.containsIgnoreCase(id, this.searchTerm) || StringUtils.containsIgnoreCase(description, this.searchTerm));
    }
}

