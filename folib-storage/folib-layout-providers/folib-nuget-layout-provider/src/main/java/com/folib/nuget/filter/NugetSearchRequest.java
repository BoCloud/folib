package com.folib.nuget.filter;

import com.folib.storage.repository.Repository;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;




@Data
@Builder
public class NugetSearchRequest {
    @NonNull
    private Repository repository;
    private String searchTerm;
    private Boolean includePreRelease;
    private Boolean shouldRemoveSemver2;
    private Integer skip;
    private Integer take;

    public boolean includePreRelease() {
        return includePreRelease != null && includePreRelease;
    }

    public boolean shouldRemoveSemVer2Results() {
        return shouldRemoveSemver2 != null && shouldRemoveSemver2;
    }

    public static class NugetSearchRequestBuilder {
        // 自定义 searchTerm 的构建逻辑（例如去空格、空值处理）
        public NugetSearchRequestBuilder searchTerm(String searchTerm) {
            if (searchTerm.startsWith("'") && searchTerm.endsWith("'") && searchTerm.length() > 1) {
                searchTerm = searchTerm.substring(1, searchTerm.length() - 1);
            }
            this.searchTerm = searchTerm;
            return this;
        }
    }

    public String toUrlParametersV2() {
        StringBuilder urlParams = new StringBuilder();
        if (searchTerm != null && !searchTerm.isEmpty()) {
            urlParams.append("searchTerm='").append(searchTerm).append("'&");
        }
        urlParams.append("includePrerelease=").append(this.includePreRelease()).append("&");
        if (skip != null) {
            urlParams.append("$skip=").append(skip).append("&");
        }
        if (take != null) {
            urlParams.append("$top=").append(take).append("&");
        }
        urlParams.append("semVerLevel=").append(this.shouldRemoveSemVer2Results() ? "1.0.0" : "2.0.0").append("&");
        return urlParams.toString();
    }

    public String toUrlParametersV3() {
        StringBuilder urlParams = new StringBuilder();
        if (searchTerm != null && !searchTerm.isEmpty()) {
            urlParams.append("q=").append(searchTerm).append("&");
        }
        if (skip != null) {
            urlParams.append("skip=").append(skip).append("&");
        }
        if (take != null) {
            urlParams.append("top=").append(take).append("&");
        }
        urlParams.append("includePrerelease=").append(this.includePreRelease()).append("&");
        urlParams.append("semVerLevel=").append(this.shouldRemoveSemVer2Results() ? "1.0.0" : "2.0.0").append("&");
        return urlParams.toString();
    }

    public String toCacheKeyV2() {
        String repositoryKey = String.format("%s:%s", repository.getStorage().getId(), repository.getId());
        String searchTermKey = searchTerm != null ? searchTerm : "";
        String includePreReleaseKey = includePreRelease != null ? "includePreRelease=" + includePreRelease : "";
        String shouldRemoveSemver2Key = shouldRemoveSemver2 != null ? "shouldRemoveSemver2=" + shouldRemoveSemver2 : "";
        String skipKey = skip != null ? "skip=" + skip : "";
        String takeKey = take != null ? "take=" + take : "";
        return String.format("V2:%s:%s:%s:%s:%s:%s",
                repositoryKey, searchTermKey, includePreReleaseKey, shouldRemoveSemver2Key, skipKey, takeKey);
    }

    public String toCacheKeyV3() {
        String repositoryKey = String.format("%s:%s", repository.getStorage().getId(), repository.getId());
        String searchTermKey = searchTerm != null ? searchTerm : "";
        String includePreReleaseKey = includePreRelease != null ? "includePreRelease=" + includePreRelease : "";
        String shouldRemoveSemver2Key = shouldRemoveSemver2 != null ? "shouldRemoveSemver2=" + shouldRemoveSemver2 : "";
        String skipKey = skip != null ? "skip=" + skip : "";
        String takeKey = take != null ? "take=" + take : "";
        return String.format("V3:%s:%s:%s:%s:%s:%s",
                repositoryKey, searchTermKey, includePreReleaseKey, shouldRemoveSemver2Key, skipKey, takeKey);
    }
}

