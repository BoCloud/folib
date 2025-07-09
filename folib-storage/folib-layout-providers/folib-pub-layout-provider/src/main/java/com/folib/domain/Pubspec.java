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
package com.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonNaming(PropertyNamingStrategy.SnakeCaseStrategy.class)
public class Pubspec implements Serializable {

    /**
     * 项目名称。
     */
    private String name;

    /**
     * 项目版本。
     */
    private String version;

    /**
     * 项目作者。
     */
    private String author;

    /**
     * 项目描述。
     */
    private String description;

    /**
     * 项目主页URL。
     */
    private String homepage;

    /**
     * 项目仓库URL。
     */
    private String repository;

    /**
     * 项目问题追踪器URL。
     */
    private String issueTracker;

    /**
     * 项目文档URL。
     */
    private String documentation;

    /**
     * 项目的依赖项。键是依赖项名称，值是表示依赖项的对象。
     */
    private Map<String, Object> dependencies;

    /**
     * 项目的开发依赖项。键是依赖项名称，值是表示依赖项的对象。
     */
    @JSONField(name = "dev_dependencies")
    private Map<String, Object> devDependencies;

    /**
     * 特定依赖项的覆盖。键是依赖项名称，值是表示依赖项覆盖的对象。
     */
    private Map<String, Object> dependencyOverrides;

    /**
     * 项目的环境设置。键是环境名称，值是表示环境设置的对象。
     */
    private Map<String, Object> environment;

    /**
     * 项目包含的可执行文件。键是可执行文件名称，值是表示可执行文件的对象。
     */
    private Map<String, Object> executables;

    /**
     * 项目支持的平台。键是平台名称，值是表示平台的对象。
     */
    private Map<String, Object> platforms;

    /**
     * 项目应该发布到的URL。
     */
    @JSONField(name = "publish_to")
    private String publishTo;

    /**
     * 项目的资金来源。
     */
    private List<String> funding;

    /**
     * 项目中的虚假秘密列表。
     */
    private List<String> falseSecrets;

    /**
     * 项目的截图。键是截图名称，值是表示截图的对象。
     */
    private Map<String, Object> screenshots;

    /**
     * 与项目相关的话题。
     */
    private List<String> topics;

    /**
     * 项目忽略的建议列表。
     */
    private List<String> ignoredAdvisories;

}
