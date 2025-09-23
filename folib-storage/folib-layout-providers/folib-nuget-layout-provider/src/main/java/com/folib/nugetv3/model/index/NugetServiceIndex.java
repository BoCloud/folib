package com.folib.nugetv3.model.index;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;


@Data
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class NugetServiceIndex {
    private String version;

    private List<Resource> resources;

    public NugetServiceIndex(String repositoryURL) {
        // 1. 先去除url末尾的 /
        while (repositoryURL.endsWith("/")) {
            repositoryURL = repositoryURL.substring(0, repositoryURL.length() - 1);
        }

        this.version = "3.0.0";
        this.resources = DefaultResources.getAllResources();
        for (Resource resource : resources) {
            String id = resource.getId();
            if (id != null && id.startsWith("@")) {
                id = id.replaceFirst("@", repositoryURL);
                resource.setId(id);
            }
        }
    }
}