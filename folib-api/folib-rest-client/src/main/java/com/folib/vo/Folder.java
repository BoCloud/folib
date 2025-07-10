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
package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

/**
 * @author veadan
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Folder {

    /**
     * 存储空间名称
     */
    private String storageId;
    /**
     * 仓库名称
     */
    private String repositoryId;
    /**
     * 名称
     */
    private String name;
    /**
     * 浏览路径
     */
    private String url;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * 最后修改时间
     */
    private Date lastModified;
    /**
     * 目录标识 true 是 false 不是
     */
    private boolean folder;
    /**
     * 文件大小
     */
    private Long size;
    /**
     * 子集
     */
    private List<Folder> children;
}
