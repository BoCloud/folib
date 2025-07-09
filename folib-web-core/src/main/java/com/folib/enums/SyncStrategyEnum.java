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
package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @Date: 2024/10/10 22:04
 * @Description: 同步用户权限策略枚举
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
public enum SyncStrategyEnum {

    SOURCE_TO_TARGET("sourceToTarget", "源同步到目标"),
    TARGET_TO_SOURCE("targetToSource", "目标同步到源"),
    TWO_WAY_SYNC("twoWaySync", "双向同步");

    private String value;

    private String description;

}
