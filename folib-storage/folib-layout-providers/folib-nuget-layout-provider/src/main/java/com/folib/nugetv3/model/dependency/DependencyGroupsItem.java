package com.folib.nugetv3.model.dependency;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.nugetv3.model.rewrite.UrlRewrite;
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
public class DependencyGroupsItem implements Serializable, UrlRewrite {
    private List<Dependency> dependencies;
    private String targetFramework;


    @Override
    public void rewrite(String v3RegistrationBaseUrl, String v2BaseUrl) {
        if (dependencies == null) {
            return;
        }
        for (Dependency dependency : dependencies) {
            dependency.rewrite(v3RegistrationBaseUrl, v2BaseUrl);
        }
    }
}
