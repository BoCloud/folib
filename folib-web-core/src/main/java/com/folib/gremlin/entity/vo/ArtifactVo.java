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
package com.folib.gremlin.entity.vo;

import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.domain.ArtifactArchiveListing;
import com.folib.domain.Vulnerability;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactVo {

    private Long id;

    private String uuid;

    private String storageId;

    private String repositoryId;

    private GenericCoordinates artifactCoordinates;

    private Set<ArtifactTag> tagSet;

    private Set<String> checksums;

    private Set<String> filenames;

    private Long sizeInBytes;

    private String lastModified;

    private String lastUsedTime;

    private String createdTime;

    private Integer downloadCount;

    private ArtifactArchiveListing artifactArchiveListing;

    private Boolean artifactFileExists;

    /**
     * 安全级别
     */
    private String safeLevel;
    /**
     * 风险凭证个数
     */
    private Integer evidenceQuantity;
    /**
     * 依赖数量
     */
    private Integer dependencyCount;
    /**
     * 有漏洞的依赖数量
     */
    private Integer dependencyVulnerabilitiesCount;
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
    /**
     * 漏洞id
     */
    private String vulnerabilityId;
    /**
     * 漏洞列表
     */
    private Set<String> vulnerabilities;
    /**
     * 漏洞列表
     */
    private Set<Vulnerability> vulnerabilitySet;
    private String name;
    private String artifactPath;
    private String size;
    private String md5;
    private String sha;
}
