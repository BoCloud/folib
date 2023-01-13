package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ConanSearchEntity {
    private String packageName;
    private String version;
    private String query;
}
