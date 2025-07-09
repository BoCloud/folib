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
package com.folib.scanner.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/9/8
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SeverityTypeEnum {

    /**
     * 严重
     */
    CRITICAL("CRITICAL", "严重"),
    /**
     * 高危
     */
    HIGH("HIGH", "高危"),
    /**
     * 中危
     */
    MEDIUM("MEDIUM", "中危"),
    /**
     * 低危
     */
    LOW("LOW", "低危"),
    ;


    /**
     * 漏洞严重程度类型
     */
    private String type;
    /**
     * 漏洞严重程度名称
     */
    private String title;

    public static String queryTitleByType(String type) {
        String title = "";
        for (SeverityTypeEnum severityTypeEnum : SeverityTypeEnum.values()) {
            if (severityTypeEnum.type.equals(type)) {
                title = severityTypeEnum.getTitle();
                break;
            }
        }
        return title;
    }

}
