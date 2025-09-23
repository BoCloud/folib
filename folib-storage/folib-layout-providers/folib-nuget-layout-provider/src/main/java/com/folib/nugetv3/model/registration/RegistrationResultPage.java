package com.folib.nugetv3.model.registration;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.rewrite.UrlRewriteWithPackageId;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RegistrationResultPage implements Serializable, UrlRewriteWithPackageId {
    @JsonProperty("@id")
    private String id;
    private int count;
    private String lower;
    private String parent;
    private String upper;
    private List<RegistrationResultPageItem> items;

    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl, String packageId) {
        this.parent = NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId);
        this.id = NuGetUrlBuilder.registrationPage(v3RegistrationBaseUrl, packageId, lower, upper);
        for (RegistrationResultPageItem item : items) {
            item.rewrite(v3RegistrationBaseUrl, v2BaseUrl, packageId);
        }
    }

}
