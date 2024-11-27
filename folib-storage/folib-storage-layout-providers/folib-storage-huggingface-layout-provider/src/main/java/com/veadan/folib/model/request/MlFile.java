package com.veadan.folib.model.request;

import java.beans.ConstructorProperties;

public class MlFile {
    private String path;

    private String sample;

    private long size;

    private String sha;


    public MlFile() {}

    @ConstructorProperties({"path", "sample", "size", "sha"})
    public MlFile(String path, String sample, long size, String sha) {
        this.path = path;
        this.sample = sample;
        this.size = size;
        this.sha = sha;
    }

    public String getPath() {
        return this.path;
    }

    public String getSample() {
        return this.sample;
    }

    public long getSize() {
        return this.size;
    }

    public String getSha() {
        return this.sha;
    }
}
