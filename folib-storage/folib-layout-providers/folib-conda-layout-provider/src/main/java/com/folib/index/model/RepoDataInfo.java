package com.folib.index.model;

import lombok.Generated;

import java.beans.ConstructorProperties;

public class RepoDataInfo {
    private String subdir;

    @Generated
    public String getSubdir() {
        return this.subdir;
    }

    @Generated
    public void setSubdir(final String subdir) {
        this.subdir = subdir;
    }

    @Generated
    public String toString() {
        return "RepoDataInfo(subdir=" + this.getSubdir() + ")";
    }

    @Generated
    public RepoDataInfo() {
    }

    @ConstructorProperties({"subdir"})
    @Generated
    public RepoDataInfo(final String subdir) {
        this.subdir = subdir;
    }
}