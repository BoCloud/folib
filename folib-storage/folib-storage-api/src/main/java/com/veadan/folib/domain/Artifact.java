package com.veadan.folib.domain;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.data.domain.DomainObject;

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
}
