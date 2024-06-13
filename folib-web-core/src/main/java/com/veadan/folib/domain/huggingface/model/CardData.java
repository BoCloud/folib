package com.veadan.folib.domain.huggingface.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

import lombok.Generated;

public class CardData {
    private List<String> language;

    private List<String> tags;

    private List<String> metrics;

    private Object inference;

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
