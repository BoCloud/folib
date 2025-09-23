package com.folib.nugetv3.model.search;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.nugetv3.model.rewrite.UrlRewrite;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.net.URI;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchResultData implements UrlRewrite {
    @JsonProperty("@id")
    private String id;
    private String registration;
    @JsonProperty("id")
    private String packageId;
    String version;
    String description;
    String summary;
    String title;
    URI iconUrl;
    URI licenseUrl;
    URI projectUrl;
    List<String> tags;
    List<String> authors;
    long totalDownloads;
    boolean verified;
    List<SearchResultDataVersion> versions;

    public void setLicenseUrl(String licenseUrl) {
        this.licenseUrl = URI.create(licenseUrl);
    }

    public void setProjectUrl(String projectUrl) {
        this.projectUrl = URI.create(projectUrl);
    }

    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl) {
        String packageId = this.packageId.toLowerCase();
        this.registration = String.format("%s/%s/index.json", this.registration, packageId);
        this.id = this.registration;
        for (SearchResultDataVersion version : versions) {
            version.rewrite(v3RegistrationBaseUrl, v2BaseUrl, packageId);
        }
    }
}
