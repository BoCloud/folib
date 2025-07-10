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
package com.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/11/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SearchArtifact {

    /**
     * 制品名称 必填
     */
    private String artifactName;

    /**
     * 元数据搜索（二者默认其中一个必填）
     */
    private String metadataSearch;

    /**
     * 存储空间id
     */
    private String storageId;
    /**
     * 仓库id
     */
    private String repositoryId;
    /**
     * 仓库id集合，逗号分隔
     */
    private String repositoryIds;
    /**
     * 开启正则匹配 true 开启 false 不开启
     */
    private Boolean regex;
    /**
     * 自定义的正则表达式
     */
    private String pattern;
    /**
     * 开始时间
     */
    private String beginDate;
    /**
     * 结束时间
     */
    private String endDate;
    /**
     * 漏洞扫描状态
     */
    private String safeLevel;
    /**
     * 校验码类型
     */
    private String digestAlgorithm;
    /**
     * 校验码
     */
    private String digest;
    /**
     * 排序字段
     */
    private String sortField;
    /**
     * 顺序
     */
    private String sortOrder;
    /**
     * 每页数据
     */
    private Integer limit;
    /**
     * 页码
     */
    private Integer page;
}
