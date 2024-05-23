package com.veadan.folib.pypi;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.util.Objects;

@Builder
@Data
@AllArgsConstructor
public class PypiSearchResult {

    private String artifactName;
    private String artifactUrl;
    private String artifactPath;
    private String storageId;
    private String repositoryId;
    private String groupName;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PypiSearchResult)) {
            return false;
        }
        PypiSearchResult that = (PypiSearchResult) o;
        return artifactName.equals(that.artifactName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(artifactName);
    }
}
