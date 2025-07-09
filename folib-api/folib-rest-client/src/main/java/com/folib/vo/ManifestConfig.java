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

import com.alibaba.fastjson.JSONObject;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ManifestConfig {

    /**
     * 基础架构
     */
    private String architecture;
    /**
     * 作者
     */
    private String author;
    /**
     * 创建时间
     */
    private String created;
    /**
     * 镜像OS
     */
    private String os;
    /**
     * 配置信息
     */
    private JSONObject config;
    /**
     * 容器
     */
    private String container;
    /**
     * 容器配置信息
     */
    private JSONObject containerConfig;
    /**
     * 制作Docker版本
     */
    private String dockerVersion;
    /**
     * 制作历史
     */
    private List<DockHistory> history;
    /**
     * 文件
     */
    private DockerRootFs rootFs;
}

