package com.veadan.folib.model.request;

import java.util.List;
import lombok.Generated;

public class MlFilesResponse {
    private List<MlFileInfo> files;

    @Generated
    public List<MlFileInfo> getFiles() {
        return this.files;
    }

    public MlFilesResponse(List<MlFileInfo> files) {
        this.files = files;
    }
}
