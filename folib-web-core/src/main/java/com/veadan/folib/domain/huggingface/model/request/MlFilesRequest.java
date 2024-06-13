package com.veadan.folib.domain.huggingface.model.request;


import java.beans.ConstructorProperties;
import java.util.List;

public class MlFilesRequest {
    private List<MlFile> files;


    public MlFilesRequest() {}

    @ConstructorProperties({"files"})
    public MlFilesRequest(List<MlFile> files) {
        this.files = files;
    }

    public List <MlFile> getFiles() {
        return this.files;
    }
}
