package com.veadan.folib.pypi;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PypiSearchResult {
    private String artifactName;
    private String artifactUrl;
}
