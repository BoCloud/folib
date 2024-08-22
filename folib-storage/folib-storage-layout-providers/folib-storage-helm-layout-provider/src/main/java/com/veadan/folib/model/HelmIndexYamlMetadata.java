package com.veadan.folib.model;


import java.util.SortedSet;
import java.util.concurrent.ConcurrentMap;

public class HelmIndexYamlMetadata {
    public String apiVersion;

    public ConcurrentMap<String, SortedSet<HelmChartMetadata>> entries;

    public String generated;

    public String getApiVersion() {
        return this.apiVersion;
    }

    public ConcurrentMap<String, SortedSet<HelmChartMetadata>> getEntries() {
        return this.entries;
    }

    public void setEntries(ConcurrentMap<String, SortedSet<HelmChartMetadata>> entries) {
        this.entries = entries;
    }

    public String getGenerated() {
        return this.generated;
    }
}
