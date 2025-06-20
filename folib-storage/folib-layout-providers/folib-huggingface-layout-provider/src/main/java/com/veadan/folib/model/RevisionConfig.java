package com.veadan.folib.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import lombok.Generated;

public class RevisionConfig {
    private List<String> architectures;

    @JsonProperty("model_type")
    private String modelType;

    @Generated
    public void setArchitectures(List<String> architectures) {
        this.architectures = architectures;
    }

    @JsonProperty("model_type")
    @Generated
    public void setModelType(String modelType) {
        this.modelType = modelType;
    }


    @Generated
    public List<String> getArchitectures() {
        return this.architectures;
    }

    @Generated
    public String getModelType() {
        return this.modelType;
    }
}

