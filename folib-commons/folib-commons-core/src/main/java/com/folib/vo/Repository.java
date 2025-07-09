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

import java.util.Set;

/**
 * @author veadan
 * @date 2022/11/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Repository {

    /**
     * 仓库名称
     */
    private String id;

    /**
     * 仓库路径
     */
    private String basedir;

    /**
     * 版本策略
     */
    private String policy;

    /**
     * 文件存储方式
     */
    private String storageProvider;

    /**
     * 布局
     */
    private String layout;

    /**
     * 仓库类型
     */
    private String type;

    /**
     * 制品大小限制(MB)
     */
    private long artifactMaxSize;

    /**
     * 删除
     */
    private boolean allowsDeletion;

    /**
     * 强制删除
     */
    private boolean allowsForceDeletion;

    /**
     * 上传部署
     */
    private boolean allowsDeployment;

    /**
     * 上传覆盖
     */
    private boolean allowsRedeployment;

    /**
     * 目录浏览
     */
    private boolean allowsDirectoryBrowsing;

    /**
     * 白名单列表
     */
    private Set<String> vulnerabilityWhites;

    /**
     * 黑名单列表
     */
    private Set<String> vulnerabilityBlacks;
}
