package com.folib.nugetv3.model.search;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.nugetv3.model.rewrite.UrlRewriteWithPackageId;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchResultDataVersion implements UrlRewriteWithPackageId {
    @JsonProperty("@id")
    private String id;
    String version;
    long downloads;

    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl, String packageId) {
        this.id = String.format("%s/%s/%s.json", v3RegistrationBaseUrl, packageId, this.version);
    }
}
