package com.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
// 文件信息
public class PackageFile {
    private String path;
    private String type; // dir/file
    // getters/setters
}
