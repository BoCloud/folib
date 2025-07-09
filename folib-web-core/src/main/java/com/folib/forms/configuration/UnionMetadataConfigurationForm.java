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
package com.folib.forms.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionMetadataConfigurationForm
        implements Serializable {

    /**
     * 元数据key
     */
    @NotBlank(message = "元数据key不能为空")
    private String metadataKey;

    /**
     * 元数据value
     */
    @NotBlank(message = "元数据value不能为空")
    private String metadataValue;


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UnionMetadataConfigurationForm)) {
            return false;
        }
        UnionMetadataConfigurationForm that = (UnionMetadataConfigurationForm) o;
        return metadataKey.equals(that.metadataKey) &&
                metadataValue.equals(that.metadataValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(metadataKey, metadataValue);
    }
}