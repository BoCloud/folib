package com.veadan.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
// 元数据根节点
public class Metadata {
    private String xmlns;
    private String xmlnsRpm;
    private int packages;
    private List<Package> packageList;
    // getters/setters
}










