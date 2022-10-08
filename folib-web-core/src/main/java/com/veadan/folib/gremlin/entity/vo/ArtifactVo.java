package com.veadan.folib.gremlin.entity.vo;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.domain.ArtifactArchiveListing;
import com.veadan.folib.domain.Vulnerability;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author leipenghui
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

    private GenericArtifactCoordinates artifactCoordinates;

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
