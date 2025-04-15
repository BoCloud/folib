package com.veadan.folib.index.model;

import lombok.Generated;

import java.beans.ConstructorProperties;

/**
 * @author LingengMa
 * @date 2025/04/09 15:03
 * @Description:
 */

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