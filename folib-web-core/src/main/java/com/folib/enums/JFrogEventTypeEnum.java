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
 * 条件类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum JFrogEventTypeEnum {

    /**
     * deployed
     */
    DEPLOYED("deployed", true),
    /**
     * copied
     */
    COPIED("copied", true),
    /**
     * deleted
     */
    DELETED("deleted", true),
    /**
     * moved
     */
    MOVED("moved", true),
    /**
     * pushed
     */
    PUSHED("pushed", true),
    ;

    /**
     * type
     */
    private String type;

    /**
     * handle
     */
    private boolean handle;

    public static boolean needHandle(String type) {
        boolean result = false;
        for (JFrogEventTypeEnum jFrogEventTypeEnum : JFrogEventTypeEnum.values()) {
            if (jFrogEventTypeEnum.type.equals(type)) {
                result = Boolean.TRUE.equals(jFrogEventTypeEnum.handle);
                break;
            }
        }
        return result;
    }

}
