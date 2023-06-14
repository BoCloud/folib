package com.veadan.folib.pypi;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PypiSearchRequest {
    private String packageName;
}
