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
package com.folib.scanner.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author veadan
 * @date 2022/9/8
 **/
@Data
@Accessors(chain = true)
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SeverityVO implements Serializable {

    /**
     * 制品路径
     */
    private String path;

    /**
     * 存储空间
     */
    private String storage;

    /**
     * 仓库名称
     */
    private String repository;

    /**
     * 报告
     */
    private String report;

    /**
     * 严重的数量
     */
    private Long critical;

    /**
     * 高危的数量
     */
    private Long high;

    /**
     * 中危的数量
     */
    private Long medium;

    /**
     * 低危的数量
     */
    private Long low;
    /**
     * 漏洞数量
     */
    private Integer vulnerabilitesCount;
    /**
     * 前端是否展示
     */
    private Boolean show;
}
