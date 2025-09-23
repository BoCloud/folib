package com.folib.nugetv3.model.search;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.nugetv3.model.rewrite.UrlRewrite;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;


@Data
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchResult implements UrlRewrite {
    int totalHits;
    private List<SearchResultData> data;

    public SearchResult(List<SearchResultData> data) {
        this.data = data != null ? data : new ArrayList<>();
        this.totalHits = data != null ? data.size() : 0;
    }

    public SearchResult() {
        this.data = new ArrayList<>();
        this.totalHits = 0;
    }

    public void merge(SearchResult other) {
        if (other != null && other.getData() != null) {
            this.data.addAll(other.getData());
            this.totalHits += other.getTotalHits();
        }
    }

    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl) {
        for (SearchResultData data : data) {
            data.rewrite(v3RegistrationBaseUrl, v2BaseUrl);
        }
    }
}
