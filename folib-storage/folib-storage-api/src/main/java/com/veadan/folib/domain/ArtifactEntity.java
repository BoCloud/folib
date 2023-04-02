package com.veadan.folib.domain;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.gremlin.adapters.DateConverter;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;
import org.springframework.util.Assert;

import javax.persistence.Transient;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Veadan
 * @author xuxinping
 */
@NodeEntity(Vertices.ARTIFACT)
public class ArtifactEntity
        extends DomainEntity implements Artifact {

    private String storageId;

    private String repositoryId;

    private String storageIdAndRepositoryId;

    @Relationship(type = Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES, direction = Relationship.OUTGOING)
    private GenericArtifactCoordinates artifactCoordinates;

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

    private Integer downloadCount = 0;

    private final ArtifactArchiveListing artifactArchiveListing = new ArtifactEntityArchiveListing();

    private Boolean artifactFileExists = Boolean.TRUE;

    /**
     * 安全级别
     */
    private String safeLevel = SafeLevelEnum.INIT.getLevel();
    /**
     * 风险凭证个数
     */
    private Integer evidenceQuantity = 0;
    /**
     * 依赖数量
     */
    private Integer dependencyCount = 0;
    /**
     * 有漏洞的依赖数量
     */
    private Integer dependencyVulnerabilitiesCount = 0;
    /**
     * 漏洞数量
     */
    private Integer vulnerabilitiesCount = 0;
    /**
     * 严重的漏洞数量
     */
    private Integer criticalVulnerabilitiesCount = 0;
    /**
     * 高危的漏洞数量
     */
    private Integer highVulnerabilitiesCount = 0;
    /**
     * 中危的漏洞数量
     */
    private Integer mediumVulnerabilitiesCount = 0;
    /**
     * 低危的漏洞数量
     */
    private Integer lowVulnerabilitiesCount = 0;
    /**
     * 被封存的漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount = 0;
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

    public ArtifactEntity() {
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
    @Transient
    public String getArtifactPath() {
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
}
