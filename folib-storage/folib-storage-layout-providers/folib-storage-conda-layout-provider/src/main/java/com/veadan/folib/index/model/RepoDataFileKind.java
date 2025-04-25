package com.veadan.folib.index.model;

import lombok.Generated;

import java.beans.ConstructorProperties;

/**
 * @author LingengMa
 * @date 2025/04/11 09:27
 * @Description:
 */

public enum RepoDataFileKind {
    REPO_DATA("repodata.json"),
    CURRENT_REPO_DATA("current_repodata.json"),
    REPO_DATA_BZ2("repodata.json.bz2"),
    CURRENT_REPO_DATA_BZ2("current_repodata.json.bz2"),
    REPO_DATA_ZST("repodata.json.zst"),
    CURRENT_REPO_DATA_ZST("current_repodata.json.zst");

    private final String fileName;

    @Generated
    public String getFileName() {
        return this.fileName;
    }

    @ConstructorProperties({"fileName"})
    @Generated
    private RepoDataFileKind(final String fileName) {
        this.fileName = fileName;
    }
}
