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
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.util.List;
import java.util.Objects;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class HelmMetadata {
    public HelmChartMetadata helmChart;

    public List<HelmDependencyMetadata> dependencies;

    public HelmMetadata() {}

    public HelmMetadata(HelmChartMetadata helmChart, List<HelmDependencyMetadata> dependencies) {
        this.helmChart = helmChart;
        this.dependencies = dependencies;
    }

    public boolean equals(Object o) {
        HelmMetadata that;
        if (this == o)
            return true;
        if (o instanceof HelmMetadata) {
            that = (HelmMetadata)o;
        } else {
            return false;
        }
        return (Objects.equals(this.helmChart, that.helmChart) && Objects.equals(this.dependencies, that.dependencies));
    }

    public int hashCode() {
        int result = (this.helmChart != null) ? this.helmChart.hashCode() : 0;
        result = 31 * result + ((this.dependencies != null) ? this.dependencies.hashCode() : 0);
        return result;
    }
}
