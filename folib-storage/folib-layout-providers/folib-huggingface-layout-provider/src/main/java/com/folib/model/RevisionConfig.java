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

