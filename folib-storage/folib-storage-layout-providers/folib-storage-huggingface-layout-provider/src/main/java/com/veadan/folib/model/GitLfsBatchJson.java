package com.veadan.folib.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Lists;
import lombok.Generated;

import java.util.List;

@JsonSerialize
@JsonInclude(JsonInclude.Include.NON_EMPTY)
@JsonIgnoreProperties(ignoreUnknown = true)
public class GitLfsBatchJson {
    private List<GitLfsJson> objects = Lists.newArrayList();

    private String operation;

    @Generated
    public List<GitLfsJson> getObjects() {
        return this.objects;
    }

    @Generated
    public void setOperation(String operation) {
        this.operation = operation;
    }

    @Generated
    public String getOperation() {
        return this.operation;
    }

    public GitLfsBatchJson(List<GitLfsJson> objects) {
        this.objects = objects;
    }

    @Generated
    public GitLfsBatchJson() {}
}

