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
package com.folib.controllers.adapter.jfrog.dto;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/2/26
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactData {

    /**
     * 名称
     */
    private String name;
    /**
     * 镜像名称
     */
    @JSONField(name = "image_name")
    private String imageName;
    /**
     * tag
     */
    private String tag;
    /**
     * 路径
     */
    private String path;
    /**
     * 仓库
     */
    @JSONField(name = "repo_key")
    private String repoKey;
    /**
     * sha256
     */
    private String sha256;
    /**
     * 大小
     */
    private long size;
    /**
     * 源路径
     */
    @JSONField(name = "source_repo_path")
    private String sourceRepoPath;
    /**
     * 目标路径
     */
    @JSONField(name = "target_repo_path")
    private String targetRepoPath;
}
