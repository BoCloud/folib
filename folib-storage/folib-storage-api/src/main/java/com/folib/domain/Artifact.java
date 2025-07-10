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
import com.folib.data.domain.DomainObject;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.Set;

public interface Artifact extends DomainObject {

    String getStorageId();

    void setStorageId(String storageId);

    String getRepositoryId();

    void setRepositoryId(String repositoryId);

    String getStorageIdAndRepositoryId();

    void setStorageIdAndRepositoryId(String storageIdAndRepositoryId);

    ArtifactCoordinates getArtifactCoordinates();

    void setArtifactCoordinates(ArtifactCoordinates artifactCoordinates);

    Set<ArtifactTag> getTagSet();

    Map<String, String> getChecksums();

    void setChecksums(Map<String, String> digestMap);

    Long getSizeInBytes();

    void setSizeInBytes(Long sizeInBytes);

    LocalDateTime getLastUpdated();

    void setLastUpdated(LocalDateTime lastUpdated);

    LocalDateTime getLastUsed();

    void setLastUsed(LocalDateTime lastUsed);

    LocalDateTime getCreated();

    void setCreated(LocalDateTime created);

    Integer getDownloadCount();

    void setDownloadCount(Integer downloadCount);

    Boolean getArtifactFileExists();

    void setArtifactFileExists(Boolean cached);

    ArtifactArchiveListing getArtifactArchiveListing();

    String getArtifactPath();

    String getSafeLevel();

    void setSafeLevel(String safeLevel);

    Integer getDependencyCount();

    void setDependencyCount(Integer dependencyCount);

    Integer getDependencyVulnerabilitiesCount();

    void setDependencyVulnerabilitiesCount(Integer dependencyVulnerabilitiesCount);

    Integer getVulnerabilitiesCount();

    void setVulnerabilitiesCount(Integer vulnerabilitiesCount);

    Integer getCriticalVulnerabilitiesCount();

    void setCriticalVulnerabilitiesCount(Integer criticalVulnerabilitiesCount);

    Integer getHighVulnerabilitiesCount();

    void setHighVulnerabilitiesCount(Integer highVulnerabilitiesCount);

    Integer getMediumVulnerabilitiesCount();

    void setMediumVulnerabilitiesCount(Integer mediumVulnerabilitiesCount);

    Integer getLowVulnerabilitiesCount();

    void setLowVulnerabilitiesCount(Integer lowVulnerabilitiesCount);

    Integer getSuppressedVulnerabilitiesCount();

    void setSuppressedVulnerabilitiesCount(Integer suppressedVulnerabilitiesCount);

    Integer getEvidenceQuantity();

    void setEvidenceQuantity(Integer evidenceQuantity);

    Set<String> getVulnerabilities();

    void setVulnerabilities(Set<String> vulnerabilities);

    Set<Vulnerability> getVulnerabilitySet();

    void setVulnerabilitySet(Set<Vulnerability> vulnerabilitySet);

    String getMetadata();

    void setMetadata(String metadata);

    Set<String> getFilePaths();

    void setFilePaths(Set<String> filePaths);

    String getScanDate();

    void setScanDate(String scanDate);

    LocalDateTime getScanDateTime();

    void setScanDateTime(LocalDateTime scanDateTime);

    String getReport();

    void setReport(String report);

    String getDependencies();

    void setDependencies(String dependencies);

    String getPromotion();

    void setPromotion(String promotion);

    Set<String> getPromotionNodes();

    void setPromotionNodes(Set<String> promotionNodes);

    Boolean getEnabled();

    void setEnabled(Boolean enabled);

    Set<Component> getComponentSet();

    void setComponentSet(Set<Component> componentSet);

    String getCreatedBy();

    void setCreatedBy(String createdBy);

    String getUpdatedBy();

    void setUpdatedBy(String updatedBy);

    void setArtifactPath(String artifactPath);

    String getPackageInfo();

    void setPackageInfo(String packageInfo);

    String getArtifactName();

    void setArtifactName(String artifactName);

    Set<String> getLayers();

    void setLayers(Set<String> layers);
}
