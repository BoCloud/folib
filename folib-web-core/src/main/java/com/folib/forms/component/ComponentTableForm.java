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
package com.folib.forms.component;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * 组件vo
 *
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ComponentTableForm implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * nativeId
     */
    private Long nativeId;
    /**
     * uuid
     */
    private String uuid;
    /**
     * 创建时间
     */
    private String created;
    /**
     * 最后更新时间
     */
    private String lastUpdated;
    /**
     * 组件名称
     */
    private String name;
    /**
     * 文件名
     */
    private String fileName;
    /**
     * 版本
     */
    private String version;
    /**
     * 组
     */
    private String groupId;
    /**
     * 描述
     */
    private String description;
    /**
     * pkg地址
     */
    private String purl;
    /**
     * url
     */
    private String url;
    /**
     * 通用平台枚举
     */
    private String cpe;
    /**
     * 通用平台枚举
     */
    private String md5sum;
    /**
     * 通用平台枚举
     */
    private String sha1sum;
    /**
     * 通用平台枚举
     */
    private String sha256sum;
    /**
     * license
     */
    private Set<String> license;
    /**
     * 漏洞
     */
    private Set<String> vulnerabilities;
    /**
     * 漏洞数量
     */
    private Integer vulnerabilitiesCount;
    /**
     * 严重的漏洞数量
     */
    private Integer criticalVulnerabilitiesCount;
    /**
     * 高危的漏洞数量
     */
    private Integer highVulnerabilitiesCount;
    /**
     * 中危的漏洞数量
     */
    private Integer mediumVulnerabilitiesCount;
    /**
     * 低危的漏洞数量
     */
    private Integer lowVulnerabilitiesCount;
    /**
     * 被封存的漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount;

}
