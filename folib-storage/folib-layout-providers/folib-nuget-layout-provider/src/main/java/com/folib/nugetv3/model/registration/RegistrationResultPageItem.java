package com.folib.nugetv3.model.registration;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.rewrite.UrlRewriteWithPackageId;
import com.folib.nugetv3.model.rewrite.UrlRewriteWithPackageId;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RegistrationResultPageItem implements Serializable, UrlRewriteWithPackageId {
    @JsonProperty("@id")
    private String id;
    private RegistrationResultCatalogEntry catalogEntry;
    private String packageContent;
    private String registration;

    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl, String packageId) {
        String version = this.catalogEntry.getVersion();
        this.id = NuGetUrlBuilder.registrationSpecificVersion(v3RegistrationBaseUrl, packageId, version);
        this.packageContent = NuGetUrlBuilder.packageContent(v2BaseUrl, packageId, version);
        this.registration = NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId);
        this.catalogEntry.rewrite(v3RegistrationBaseUrl, v2BaseUrl);

    }
}
