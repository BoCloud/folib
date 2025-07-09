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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2023/10/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CacheSettings {

    /**
     * 启用缓存 true 是 false 否
     */
    private boolean enabled;

    /**
     * 缓存目录
     */
    private String directoryPath;

    /**
     * 最小缓存值
     */
    private String minSize;

    /**
     * 最小缓存值的单位
     */
    private String minSizeUnit;

    /**
     * 最大缓存值
     */
    private String maxSize;

    /**
     * 最大缓存值的单位
     */
    private String maxSizeUnit;

    /**
     * 缓存容量
     */
    private String size;

    /**
     * 缓存容量单位 GB TB
     */
    private String sizeUnit;

    /**
     * 清理条件
     */
    private int clearCondition;

    /**
     * 清理比例
     */
    private int clearProportion;
}
