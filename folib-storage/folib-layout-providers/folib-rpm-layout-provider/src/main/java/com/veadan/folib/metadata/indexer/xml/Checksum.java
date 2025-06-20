package com.veadan.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
// 校验和信息
public class Checksum {
    private String type;
    private String pkgid;
    private String value;
    // getters/setters
}
