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
package com.folib.event.privilege;

/**
 * @author Veadan
 */
public enum PrivilegeEventTypeEnum {

    /**
     * Occurs when an artifact directory has been deleted.
     */
    EVENT_USER_SYNC(1),
    /**
     * Occurs when an metadata operation has update.
     */
    EVENT_USER_GROUP_SYNC(2),
    /**
     * Occurs when an artifact download operation has blocked.
     */
    EVENT_ROLE_SYNC(3),
    /**
     * Occurs when an artifact cache operation has need.
     */
    EVENT_RESOURCE_SYNC(4),
    EVENT_ALL_SYNC(5),
    EVENT_DELETE_USER_SYNC(6),
    EVENT_DELETE_USER_GROUP_SYNC(7),
    EVENT_DELETE_ROLE_SYNC(8),
    EVENT_DELETE_RESOURCE_SYNC(9),
    ;


    private int type;


    PrivilegeEventTypeEnum(int type) {
        this.type = type;
    }

    public int getType() {
        return type;
    }

    /**
     * 根据类型查询枚举类型
     *
     * @param type 类型
     * @return 枚举类型
     */
    public static PrivilegeEventTypeEnum queryPrivilegeEventTypeEnumByType(int type) {
        for (PrivilegeEventTypeEnum artifactEventTypeEnum : PrivilegeEventTypeEnum.values()) {
            if (artifactEventTypeEnum.getType() == type) {
                return artifactEventTypeEnum;
            }
        }
        return null;
    }
}
