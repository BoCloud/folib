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

import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.adapters.DateConverter;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.typeconversion.Convert;
import org.springframework.util.Assert;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;

/**
 * 组件
 *
 * @author veadan
 **/
@NodeEntity(Vertices.COMPONENT)
public class ComponentEntity extends DomainEntity implements Component {

    /**
     * 创建时间
     */
    @Convert(DateConverter.class)
    private LocalDateTime created;
    /**
     * 更新时间
     */
    @Convert(DateConverter.class)
    private LocalDateTime lastUpdated;
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

    @Override
    public LocalDateTime getCreated() {
        return created;
    }

    @Override
    public void setCreated(LocalDateTime created) {
        this.created = created;
    }

    @Override
    public LocalDateTime getLastUpdated() {
        return lastUpdated;
    }

    @Override
    public void setLastUpdated(LocalDateTime lastUpdated) {
        this.lastUpdated = lastUpdated;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getFileName() {
        return fileName;
    }

    @Override
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public String getVersion() {
        return version;
    }

    @Override
    public void setVersion(String version) {
        this.version = version;
    }

    @Override
    public String getGroupId() {
        return groupId;
    }

    @Override
    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public String getPurl() {
        return purl;
    }

    @Override
    public void setPurl(String purl) {
        this.purl = purl;
    }

    @Override
    public String getUrl() {
        return url;
    }

    @Override
    public void setUrl(String url) {
        this.url = url;
    }

    @Override
    public String getCpe() {
        return cpe;
    }

    @Override
    public void setCpe(String cpe) {
        this.cpe = cpe;
    }

    @Override
    public String getMd5sum() {
        return md5sum;
    }

    @Override
    public void setMd5sum(String md5sum) {
        this.md5sum = md5sum;
    }

    @Override
    public String getSha1sum() {
        return sha1sum;
    }

    @Override
    public void setSha1sum(String sha1sum) {
        this.sha1sum = sha1sum;
    }

    @Override
    public String getSha256sum() {
        return sha256sum;
    }

    @Override
    public void setSha256sum(String sha256sum) {
        this.sha256sum = sha256sum;
    }

    @Override
    public Set<String> getLicense() {
        return license;
    }

    @Override
    public void setLicense(Set<String> license) {
        this.license = license;
    }

    @Override
    public Set<String> getVulnerabilities() {
        return vulnerabilities;
    }

    @Override
    public void setVulnerabilities(Set<String> vulnerabilities) {
        this.vulnerabilities = vulnerabilities;
    }

    @Override
    public Integer getVulnerabilitiesCount() {
        return vulnerabilitiesCount;
    }

    @Override
    public void setVulnerabilitiesCount(Integer vulnerabilitiesCount) {
        this.vulnerabilitiesCount = vulnerabilitiesCount;
    }

    @Override
    public Integer getCriticalVulnerabilitiesCount() {
        return criticalVulnerabilitiesCount;
    }

    @Override
    public void setCriticalVulnerabilitiesCount(Integer criticalVulnerabilitiesCount) {
        this.criticalVulnerabilitiesCount = criticalVulnerabilitiesCount;
    }

    @Override
    public Integer getHighVulnerabilitiesCount() {
        return highVulnerabilitiesCount;
    }

    @Override
    public void setHighVulnerabilitiesCount(Integer highVulnerabilitiesCount) {
        this.highVulnerabilitiesCount = highVulnerabilitiesCount;
    }

    @Override
    public Integer getMediumVulnerabilitiesCount() {
        return mediumVulnerabilitiesCount;
    }

    @Override
    public void setMediumVulnerabilitiesCount(Integer mediumVulnerabilitiesCount) {
        this.mediumVulnerabilitiesCount = mediumVulnerabilitiesCount;
    }

    @Override
    public Integer getLowVulnerabilitiesCount() {
        return lowVulnerabilitiesCount;
    }

    @Override
    public void setLowVulnerabilitiesCount(Integer lowVulnerabilitiesCount) {
        this.lowVulnerabilitiesCount = lowVulnerabilitiesCount;
    }

    @Override
    public Integer getSuppressedVulnerabilitiesCount() {
        return suppressedVulnerabilitiesCount;
    }

    @Override
    public void setSuppressedVulnerabilitiesCount(Integer suppressedVulnerabilitiesCount) {
        this.suppressedVulnerabilitiesCount = suppressedVulnerabilitiesCount;
    }

    public ComponentEntity() {
    }

    public ComponentEntity(String sha1sum) {
        Assert.notNull(sha1sum, "Component uuid should not be null");
        setUuid(sha1sum);
        setSha1sum(sha1sum);
        int zero = 0;
        if (Objects.isNull(this.vulnerabilitiesCount)) {
            this.vulnerabilitiesCount = zero;
        }
        if (Objects.isNull(this.criticalVulnerabilitiesCount)) {
            this.criticalVulnerabilitiesCount = zero;
        }
        if (Objects.isNull(this.highVulnerabilitiesCount)) {
            this.highVulnerabilitiesCount = zero;
        }
        if (Objects.isNull(this.mediumVulnerabilitiesCount)) {
            this.mediumVulnerabilitiesCount = zero;
        }
        if (Objects.isNull(this.lowVulnerabilitiesCount)) {
            this.lowVulnerabilitiesCount = zero;
        }
        if (Objects.isNull(this.suppressedVulnerabilitiesCount)) {
            this.suppressedVulnerabilitiesCount = zero;
        }
    }


}
