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


import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;
import lombok.Generated;


public class CargoSearchModel {
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<CargoSearchEntriesModel> crates;

    private CargoSearchSummaryModel meta;

    @Generated
    public CargoSearchModel() {}

    @Generated
    public void setCrates(List<CargoSearchEntriesModel> crates) {
        this.crates = crates;
    }

    @Generated
    public void setMeta(CargoSearchSummaryModel meta) {
        this.meta = meta;
    }

    @Generated
    public List<CargoSearchEntriesModel> getCrates() {
        return this.crates;
    }

    @Generated
    public CargoSearchSummaryModel getMeta() {
        return this.meta;
    }

    public CargoSearchModel(List<CargoSearchEntriesModel> crates, int total) {
        this.crates = crates;
        this.meta = new CargoSearchSummaryModel(total);
    }
}

