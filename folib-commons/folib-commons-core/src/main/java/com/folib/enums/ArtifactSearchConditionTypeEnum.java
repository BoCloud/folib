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
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/11
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactSearchConditionTypeEnum {


    /**
     * match
     */
    MATCH("match", "$match"),
    /**
     * nmatch
     */
    N_MATCH("nmatch", "$nmatch"),
    /**
     * EQ
     */
    EQ("eq", "$eq"),
    /**
     * ne
     */
    NE("ne", "$ne"),
    /**
     * gte
     */
    GTE("gte", "$gte"),
    /**
     * lte
     */
    LTE("lte", "$lte"),
    /**
     * or
     */
    OR("or", "$or"),
    /**
     * and
     */
    AND("and", "$and"),
    ;

    private String type;

    private String source;

    public static ArtifactSearchConditionTypeEnum queryTypeEnum(String value) {
        if (StringUtils.isBlank(value)) {
            return MATCH;
        }
        ArtifactSearchConditionTypeEnum result = null;
        for (ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum : ArtifactSearchConditionTypeEnum.values()) {
            if (artifactSearchConditionTypeEnum.getType().equals(value)) {
                result = artifactSearchConditionTypeEnum;
                break;
            }
        }
        if (Objects.isNull(result)) {
            return MATCH;
        }
        return result;
    }

    public static ArtifactSearchConditionTypeEnum queryTypeEnumBySource(String source) {
        if (StringUtils.isBlank(source)) {
            return MATCH;
        }
        ArtifactSearchConditionTypeEnum result = null;
        for (ArtifactSearchConditionTypeEnum artifactSearchConditionTypeEnum : ArtifactSearchConditionTypeEnum.values()) {
            if (artifactSearchConditionTypeEnum.getSource().equals(source)) {
                result = artifactSearchConditionTypeEnum;
                break;
            }
        }
        if (Objects.isNull(result)) {
            return MATCH;
        }
        return result;
    }
}
