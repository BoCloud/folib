package com.veadan.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
// 依赖关系
public class Dependency {
    private String name;
    private String flags;
    private String epoch;
    private String ver;
    private String rel;
    // getters/setters
}
