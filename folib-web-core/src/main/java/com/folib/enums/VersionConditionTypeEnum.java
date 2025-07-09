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

import com.google.common.collect.Lists;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Collections;
import java.util.List;

/**
 * 制品仓库类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum VersionConditionTypeEnum {

    /**
     * 大于
     */
    GT(">", Lists.newArrayList(1), "("),
    /**
     * 大于等于
     */
    GE(">=", Lists.newArrayList(1, 0), "["),
    /**
     * 小于
     */
    LT("<", Lists.newArrayList(-1), ")"),
    /**
     * 小于等于
     */
    LE("<=", Lists.newArrayList(-1, 0), "]"),
    /**
     * 等于
     */
    EQ("=", Lists.newArrayList(0), "="),
    ;

    /**
     * name
     */
    private String condition;
    /**
     * value
     */
    private List<Integer> value;
    /**
     * symbol
     */
    private String symbol;

    public static List<Integer> queryValue(String condition) {
        for (VersionConditionTypeEnum item : VersionConditionTypeEnum.values()) {
            if (item.condition.equals(condition)) {
                return item.value;
            }
        }
        return Collections.emptyList();
    }

    public static List<Integer> queryValueBySymbol(String symbol) {
        for (VersionConditionTypeEnum item : VersionConditionTypeEnum.values()) {
            if (item.symbol.equals(symbol)) {
                return item.value;
            }
        }
        return Collections.emptyList();
    }

}
