package com.folib.metadata.indexer.xml;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
@Getter
@Setter
public class Format {
    private String license;
    private String vendor;
    private String group;
    private String buildHost;
    private String sourcerpm;
    private List<Entry> requires;
    private List<Entry> provides;
    private List<Entry> conflicts;
    private List<Entry> obsoletes;
    private HeaderRange headerRange;
    private List<PackageFile> files;

    // Getters & Setters
}
