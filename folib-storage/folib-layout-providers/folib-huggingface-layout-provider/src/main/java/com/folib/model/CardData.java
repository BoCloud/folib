package com.folib.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Generated;
@JsonSerialize
public class CardData {
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @JsonIgnoreProperties(ignoreUnknown = true)
    private List<String> language;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @JsonIgnoreProperties(ignoreUnknown = true)
    private List<String> tags;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @JsonIgnoreProperties(ignoreUnknown = true)
    private List<String> metrics;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @JsonIgnoreProperties(ignoreUnknown = true)
    private Object inference;
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    @JsonIgnoreProperties(ignoreUnknown = true)
    private String license;

    @Generated
    public void setLanguage(List<String> language) {
        this.language = language;
    }

    @Generated
    public void setTags(List<String> tags) {
        this.tags = tags;
    }

    @Generated
    public void setMetrics(List<String> metrics) {
        this.metrics = metrics;
    }

    @Generated
    public void setInference(Object inference) {
        this.inference = inference;
    }


    @Generated
    public List<String> getLanguage() {
        return this.language;
    }

    @Generated
    public List<String> getTags() {
        return this.tags;
    }

    @Generated
    public List<String> getMetrics() {
        return this.metrics;
    }

    @Generated
    public Object getInference() {
        return this.inference;
    }

    @Generated
    public String getLicense() {
        return this.license;
    }

    @JsonProperty("license")
    public void setLicense(List<String> licenses) {
        this.license = String.join(",", (Iterable) licenses);
    }

    public void setLicense(String license) {
        this.license = license;
    }
}
