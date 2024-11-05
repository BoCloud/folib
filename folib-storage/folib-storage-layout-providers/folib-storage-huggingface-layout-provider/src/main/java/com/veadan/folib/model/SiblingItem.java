package com.veadan.folib.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Generated;

public class SiblingItem {
    @JsonProperty("rfilename")
    private String fileName;

    @JsonProperty("rfilename")
    @Generated
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    @Generated
    public String getFileName() {
        return this.fileName;
    }
}
