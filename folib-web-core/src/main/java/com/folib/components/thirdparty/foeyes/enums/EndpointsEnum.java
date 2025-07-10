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
package com.folib.components.thirdparty.foeyes.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum EndpointsEnum {

    /**
     * 登录
     */
    USER_LOGIN("USER_LOGIN", "/api/v1/user/login"),

    /**
     * 创建项目
     */
    CREATE_PROJECT("CREATE_PROJECT", "/api/v1/project"),

    /**
     * 依据项目id查询项目
     */
    QUERY_PROJECT("QUERY_PROJECT", "/api/v1/project/%s"),

    /**
     * 按其名称和版本返回特定项目
     */
    LOOKUP_PROJECT("LOOKUP_PROJECT", "/api/v1/project/lookup"),

    /**
     * 上传bom
     */
    BOM_UPLOAD("BOM_UPLOAD", "/api/v1/bom"),

    ;

    /**
     * name
     */
    private String name;

    /**
     * path
     */
    private String path;
}
