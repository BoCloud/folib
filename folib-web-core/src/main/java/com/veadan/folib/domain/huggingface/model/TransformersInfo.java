package com.veadan.folib.domain.huggingface.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Generated;
@JsonSerialize
@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonIgnoreProperties(ignoreUnknown = true)
public class TransformersInfo {
    @JsonProperty("auto_model")
    private String autoModel;

    @JsonProperty("pipeline_tag")
    private String pipelineTag;

    private String processor;

    @JsonProperty("auto_model")
    @Generated
    public void setAutoModel(String autoModel) {
        this.autoModel = autoModel;
    }

    @JsonProperty("pipeline_tag")
    @Generated
    public void setPipelineTag(String pipelineTag) {
        this.pipelineTag = pipelineTag;
    }

    @Generated
    public void setProcessor(String processor) {
        this.processor = processor;
    }


    @Generated
    public String getAutoModel() {
        return this.autoModel;
    }

    @Generated
    public String getPipelineTag() {
        return this.pipelineTag;
    }

    @Generated
    public String getProcessor() {
        return this.processor;
    }
}

