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
package com.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.folib.enums.ArtifactMetadataEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/11/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadata {

    /**
     * 元数据值
     */
    @JSONField(ordinal = 1)
    private String value;
    /**
     * 元数据类型
     *
     * @see ArtifactMetadataEnum
     */
    @JSONField(ordinal = 2)
    private String type;
    /**
     * 前端是否展示
     */
    @JSONField(ordinal = 3)
    private Integer viewShow;
    /**
     * 展示位置
     */
    @JSONField(ordinal = 4)
    private String location;
}
