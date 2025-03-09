package com.veadan.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Size {
    private long packageSize;
    private long installed;
    private long archive;

    // Getters & Setters (注意属性名与XML字段的映射)
}


