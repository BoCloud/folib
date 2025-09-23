package com.folib.nugetv3.model.registration;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.dependency.DependencyGroupsItem;
import com.folib.nugetv3.model.deserializers.AuthorsDeserializer;
import com.folib.nugetv3.model.rewrite.UrlRewrite;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.opencypher.v9_0.rewriting.Deprecation;

import java.io.Serializable;
import java.net.URI;
import java.util.List;



@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RegistrationResultCatalogEntry implements Serializable, UrlRewrite {
    @JsonDeserialize(
            using = AuthorsDeserializer.class
    )
    private String authors;
    private String description;
    private URI iconUrl;
    private Deprecation deprecation;
    @JsonProperty("id")
    private String packageId;
    private String language;
    private String licenseUrl;
    private Boolean listed;
    private String packageContent;
    private String projectUrl;
    private boolean requireLicenseAcceptance;
    private String summary;
    private List<String> tags;
    private String title;
    private String version;
    private List<DependencyGroupsItem> dependencyGroups;

    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl) {
        // 1. Rewrite the download URL
        String packageId = this.packageId.toLowerCase();
        this.packageContent = NuGetUrlBuilder.packageContent(v2BaseUrl, packageId, version);
        // 2. Rewrite the DependencyGroups
        if (dependencyGroups == null) {
            return;
        }
        for (DependencyGroupsItem dependencyGroup : dependencyGroups) {
            dependencyGroup.rewrite(v3RegistrationBaseUrl, v2BaseUrl);
        }
    }
}
