package com.folib.index.model;

import lombok.Generated;

import java.util.Map;


public class MetaYaml {
    private Map<String, Object> build;

    @Generated
    public MetaYaml() {
    }

    @Generated
    public Map<String, Object> getBuild() {
        return this.build;
    }

    @Generated
    public void setBuild(final Map<String, Object> build) {
        this.build = build;
    }

    @Generated
    public String toString() {
        return "MetaYaml(build=" + this.getBuild() + ")";
    }
}
