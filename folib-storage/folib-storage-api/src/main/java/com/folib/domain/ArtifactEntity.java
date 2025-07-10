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

import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Vertices;
import com.folib.enums.SafeLevelEnum;
import com.folib.gremlin.adapters.DateConverter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;
import org.springframework.util.Assert;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 * @author veadan
 */
@Slf4j
@NodeEntity(Vertices.ARTIFACT)
public class ArtifactEntity
        extends DomainEntity implements Artifact {

    private static final long serialVersionUID = -1615799903531010501L;

    private String storageId;

    private String repositoryId;

    private String storageIdAndRepositoryId;

    @Relationship(type = Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES, direction = Relationship.OUTGOING)
    private GenericCoordinates artifactCoordinates;

    @Relationship(type = Edges.ARTIFACT_HAS_TAGS, direction = Relationship.OUTGOING)
    private Set<ArtifactTag> tagSet;

    private Set<String> checksums = new HashSet<>();

    private Set<String> filenames = new LinkedHashSet<>();

    private Long sizeInBytes;

    @Convert(DateConverter.class)
    private LocalDateTime lastUpdated;

    @Convert(DateConverter.class)
    private LocalDateTime lastUsed;

    @Convert(DateConverter.class)
    private LocalDateTime created;

    private Integer downloadCount;

    private final ArtifactArchiveListing artifactArchiveListing = new ArtifactEntityArchiveListing();

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
     * 漏洞列表
     */
    private Set<String> vulnerabilities = new LinkedHashSet<>();
    /**
     * 漏洞列表
     */
    @Relationship(type = Edges.ARTIFACT_HAS_VULNERABILITIES, direction = Relationship.OUTGOING)
    private Set<Vulnerability> vulnerabilitySet;

    /**
     * 元数据
     */
    private String metadata;

    /**
     * 制品路径
     */
    private Set<String> filePaths = new LinkedHashSet<>();
    /**
     * 扫描日期
     */
    private String scanDate;
    /**
     * 扫描时间
     */
    @Convert(DateConverter.class)
    private LocalDateTime scanDateTime;
    /**
     * 依赖
     */
    private String dependencies;
    /**
     * 扫描报告
     */
    private String report;
    /**
     * 晋级状态
     */
    private String promotion;
    /**
     * 晋级节点
     */
    private Set<String> promotionNodes;
    /**
     * 状态 true 可用 false 禁用
     */
    private Boolean enabled;
    /**
     * 创建人
     */
    private String createdBy;
    /**
     * 更新人
     */
    private String updatedBy;
    /**
     * 制品名称
     */
    private String artifactName;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * 综合信息
     */
    private String packageInfo;
    /**
     * Layers
     */
    private Set<String> layers = new LinkedHashSet<>();

    /**
     * 组件列表
     */
    @Relationship(type = Edges.ARTIFACT_HAS_COMPONENTS, direction = Relationship.OUTGOING)
    private Set<Component> componentSet;


    public ArtifactEntity() {
    }

    public ArtifactEntity(Long nativeId, String storageId,
                          String repositoryId, String uuid, ArtifactCoordinates artifactCoordinates) {
        if (Objects.nonNull(nativeId)) {
            setNativeId(nativeId);
        }
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.artifactCoordinates = artifactCoordinates;
        setUuid(uuid);
        setArtifactPath(getArtifactPath());
        setArtifactName(FilenameUtils.getName(getArtifactPath()));
    }

    public ArtifactEntity(String storageId,
                          String repositoryId,
                          ArtifactCoordinates artifactCoordinates) {
        Assert.notNull(artifactCoordinates, "artifactCoordinates cannot be empty");

        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        this.artifactCoordinates = artifactCoordinates;
        setUuid(String.format("%s-%s-%s", getStorageId(), getRepositoryId(), getArtifactCoordinates().buildPath()));
        setArtifactPath(getArtifactPath());
        setArtifactName(FilenameUtils.getName(getArtifactPath()));
        if (Objects.isNull(this.downloadCount)) {
            this.downloadCount = 0;
        }
        if (Objects.isNull(this.artifactFileExists)) {
            this.artifactFileExists = true;
        }
        if (StringUtils.isBlank(this.safeLevel)) {
            this.safeLevel = SafeLevelEnum.INIT.getLevel();
        }
        if (Objects.isNull(this.evidenceQuantity)) {
            this.evidenceQuantity = 0;
        }
        if (Objects.isNull(this.dependencyCount)) {
            this.dependencyCount = 0;
        }
        if (Objects.isNull(this.dependencyVulnerabilitiesCount)) {
            this.dependencyVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.vulnerabilitiesCount)) {
            this.vulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.criticalVulnerabilitiesCount)) {
            this.criticalVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.highVulnerabilitiesCount)) {
            this.highVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.mediumVulnerabilitiesCount)) {
            this.mediumVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.lowVulnerabilitiesCount)) {
            this.lowVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.suppressedVulnerabilitiesCount)) {
            this.suppressedVulnerabilitiesCount = 0;
        }
        if (Objects.isNull(this.enabled)) {
            this.enabled = true;
        }
    }

    @Override
    public String getStorageId() {
        return storageId;
    }

    @Override
    public void setStorageId(String storageId) {
        this.storageId = storageId;
    }

    @Override
    public String getRepositoryId() {
        return repositoryId;
    }

    @Override
    public void setRepositoryId(String repositoryId) {
        this.repositoryId = repositoryId;
    }

    @Override
    public String getStorageIdAndRepositoryId() {
        return storageIdAndRepositoryId;
    }

    @Override
    public void setStorageIdAndRepositoryId(String storageIdAndRepositoryId) {
        this.storageIdAndRepositoryId = storageIdAndRepositoryId;
    }

    @Override
    public ArtifactCoordinates getArtifactCoordinates() {
        if (artifactCoordinates instanceof ArtifactCoordinates) {
            return (ArtifactCoordinates) artifactCoordinates;
        }

        return (ArtifactCoordinates) artifactCoordinates.getHierarchyChild();
    }

    @Override
    public void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        this.artifactCoordinates = artifactCoordinates;
    }

    @Override
    public Set<ArtifactTag> getTagSet() {
        return tagSet = Optional.ofNullable(tagSet).orElse(new HashSet<>());
    }

    public void setTagSet(Set<ArtifactTag> tagSet) {
        this.tagSet = tagSet;
    }

    @Override
    public Map<String, String> getChecksums() {
        return checksums.stream().filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toMap(e -> e.substring(1, e.indexOf("}")),
                        e -> e.substring(e.indexOf("}") + 1)));
    }

    @Override
    public void setChecksums(Map<String, String> checksums) {
        this.checksums.clear();
        this.checksums.addAll(checksums.entrySet()
                .stream()
                .map(e -> "{" + e.getKey() + "}" + e.getValue())
                .collect(Collectors.toSet()));
    }

    public void addChecksums(Set<String> checksums) {
        if (checksums == null) {
            return;
        }
        checksums.stream()
                .filter(e -> e.startsWith("{"))
                .filter(e -> e.indexOf("}") > 1)
                .filter(e -> !e.endsWith("}"))
                .forEach(this.checksums::add);
    }

    @Override
    public Long getSizeInBytes() {
        return sizeInBytes;
    }

    @Override
    public void setSizeInBytes(Long sizeInBytes) {
        this.sizeInBytes = sizeInBytes;
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
    public LocalDateTime getLastUsed() {
        return lastUsed;
    }

    @Override
    public void setLastUsed(LocalDateTime lastUsed) {
        this.lastUsed = lastUsed;
    }

    @Override
    public LocalDateTime getCreated() {
        return created;
    }

    @Override
    public void setCreated(LocalDateTime created) {
        this.created = created;
    }

    @Override
    public Integer getDownloadCount() {
        return downloadCount;
    }

    @Override
    public void setDownloadCount(Integer downloadCount) {
        this.downloadCount = downloadCount;
    }

    @Override
    public ArtifactArchiveListing getArtifactArchiveListing() {
        return artifactArchiveListing;
    }

    @Override
    public Boolean getArtifactFileExists() {
        return artifactFileExists;
    }

    @Override
    public void setArtifactFileExists(Boolean cached) {
        this.artifactFileExists = cached;
    }

    @Override
    public String getArtifactPath() {
        if (StringUtils.isNotBlank(artifactPath)) {
            return artifactPath;
        }
        return Optional.of(getArtifactCoordinates())
                .map(c -> c.buildPath())
                .orElseThrow(() -> new IllegalStateException("ArtifactCoordinates required to be set."));
    }

    @Override
    public String getSafeLevel() {
        return safeLevel;
    }

    @Override
    public void setSafeLevel(String safeLevel) {
        this.safeLevel = safeLevel;
    }

    @Override
    public Integer getDependencyCount() {
        return dependencyCount;
    }

    @Override
    public void setDependencyCount(Integer dependencyCount) {
        this.dependencyCount = dependencyCount;
    }

    @Override
    public Integer getDependencyVulnerabilitiesCount() {
        return dependencyVulnerabilitiesCount;
    }

    @Override
    public void setDependencyVulnerabilitiesCount(Integer dependencyVulnerabilitiesCount) {
        this.dependencyVulnerabilitiesCount = dependencyVulnerabilitiesCount;
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

    public class ArtifactEntityArchiveListing implements ArtifactArchiveListing {
        @Override
        public Set<String> getFilenames() {
            return ArtifactEntity.this.filenames.stream().filter(e -> !e.isEmpty()).collect(Collectors.toSet());
        }

        @Override
        public void setFilenames(final Set<String> filenames) {
            ArtifactEntity.this.filenames = filenames;
        }

    }

    @Override
    public Integer getEvidenceQuantity() {
        return evidenceQuantity;
    }

    @Override
    public void setEvidenceQuantity(Integer evidenceQuantity) {
        this.evidenceQuantity = evidenceQuantity;
    }

    @Override
    public Set<String> getVulnerabilities() {
        return Optional.ofNullable(vulnerabilities).orElse(new LinkedHashSet<>());
    }

    @Override
    public void setVulnerabilities(Set<String> vulnerabilities) {
        this.vulnerabilities = vulnerabilities;
    }

    @Override
    public Set<Vulnerability> getVulnerabilitySet() {
        return vulnerabilitySet = Optional.ofNullable(vulnerabilitySet).orElse(new HashSet<>());
    }

    @Override
    public void setVulnerabilitySet(Set<Vulnerability> vulnerabilitySet) {
        this.vulnerabilitySet = vulnerabilitySet;
    }

    @Override
    public String getMetadata() {
        return metadata;
    }

    @Override
    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }

    @Override
    public Set<String> getFilePaths() {
        return filePaths;
    }

    @Override
    public void setFilePaths(Set<String> filePaths) {
        this.filePaths = filePaths;
    }

    @Override
    public String getScanDate() {
        return scanDate;
    }

    @Override
    public void setScanDate(String scanDate) {
        this.scanDate = scanDate;
    }

    @Override
    public LocalDateTime getScanDateTime() {
        return scanDateTime;
    }

    @Override
    public void setScanDateTime(LocalDateTime scanDateTime) {
        this.scanDateTime = scanDateTime;
    }

    @Override
    public String getReport() {
        return report;
    }

    @Override
    public void setReport(String report) {
        this.report = report;
    }

    @Override
    public String getDependencies() {
        return dependencies;
    }

    @Override
    public void setDependencies(String dependencies) {
        this.dependencies = dependencies;
    }

    @Override
    public String getPromotion() {
        return promotion;
    }

    @Override
    public void setPromotion(String promotion) {
        this.promotion = promotion;
    }

    @Override
    public Set<String> getPromotionNodes() {
        return promotionNodes;
    }

    @Override
    public void setPromotionNodes(Set<String> promotionNodes) {
        this.promotionNodes = promotionNodes;
    }

    @Override
    public Boolean getEnabled() {
        return enabled;
    }

    @Override
    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    @Override
    public Set<Component> getComponentSet() {
        return componentSet = Optional.ofNullable(componentSet).orElse(Collections.emptySet());
    }

    @Override
    public void setComponentSet(Set<Component> componentSet) {
        this.componentSet = componentSet;
    }

    @Override
    public String getCreatedBy() {
        return createdBy;
    }

    @Override
    public void setCreatedBy(String createdBy) {
        if (StringUtils.isNotBlank(this.createdBy) && !this.createdBy.equals(createdBy)) {
            return;
        }
        this.createdBy = createdBy;
    }

    @Override
    public String getUpdatedBy() {
        return updatedBy;
    }

    @Override
    public void setUpdatedBy(String updatedBy) {
        this.updatedBy = updatedBy;
    }

    @Override
    public void setArtifactPath(String artifactPath) {
        if (StringUtils.isNotBlank(this.artifactPath) && !this.artifactPath.equals(artifactPath)) {
            return;
        }
        this.artifactPath = artifactPath;
    }

    @Override
    public String getPackageInfo() {
        return packageInfo;
    }

    @Override
    public void setPackageInfo(String packageInfo) {
        this.packageInfo = packageInfo;
    }

    @Override
    public String getArtifactName() {
        return artifactName;
    }

    @Override
    public void setArtifactName(String artifactName) {
        if (StringUtils.isNotBlank(this.artifactName) && !this.artifactName.equals(artifactName)) {
            return;
        }
        this.artifactName = artifactName;
    }

    @Override
    public Set<String> getLayers() {
        return layers;
    }

    @Override
    public void setLayers(Set<String> layers) {
        this.layers = layers;
    }
}
