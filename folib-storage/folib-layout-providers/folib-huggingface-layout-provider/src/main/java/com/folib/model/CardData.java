/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
