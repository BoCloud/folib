package com.veadan.folib.domain.huggingface.model.request;

import java.beans.ConstructorProperties;
import lombok.Generated;

public class MlFileInfo {
    private String path;

    private String uploadMode;

    private boolean shouldIgnore;

    @Generated
    public MlFileInfo() {}

    @ConstructorProperties({"path", "uploadMode", "shouldIgnore"})
    @Generated
    public MlFileInfo(String path, String uploadMode, boolean shouldIgnore) {
        this.path = path;
        this.uploadMode = uploadMode;
        this.shouldIgnore = shouldIgnore;
    }

    @Generated
    public String getPath() {
        return this.path;
    }

    @Generated
    public String getUploadMode() {
        return this.uploadMode;
    }

    @Generated
    public boolean isShouldIgnore() {
        return this.shouldIgnore;
    }
}
