package com.folib.nugetv3.model.dependency;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nugetv3.model.rewrite.UrlRewrite;
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
public class Dependency implements Serializable, UrlRewrite {
    @JsonProperty("id")
    private String packageId;
    private String range;
    private String registration;

    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl) {
        String id = this.packageId.toLowerCase();
        this.registration = NuGetUrlBuilder.registration(v3RegistrationBaseUrl, id);
    }
}
