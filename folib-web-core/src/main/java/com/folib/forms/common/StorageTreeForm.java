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
package com.folib.forms.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 树结构form
 *
 * @author leipenghui
 * @date 2022/11/21
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class StorageTreeForm implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    private String id;
    /**
     * 名称
     */
    private String name;
    /**
     * 制品库类型
     */
    private String artifactoryRepositoryType;
    /**
     * key
     */
    private String key;
    /**
     * 类型
     */
    private String type;
    /**
     * 布局
     */
    private String layout;
    /**
     * 仓库可见范围
     */
    private Integer scope;
    /**
     * 子集
     */
    private List<StorageTreeForm> children;
}
