package com.veadan.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Artifact {

    /**
     * 制品路径
     */
    private String uuid;
    /**
     * 存储空间名称
     */
    private String storageId;
    /**
     * 仓库名称
     */
    private String repositoryId;
    /**
     * 制品路径
     */
    private String artifactPath;
    /**
     * checksums
     */
    private Map<String, String> checksums;
    /**
     * artifactArchiveListing
     */
    private ArtifactArchiveListing artifactArchiveListing;
    /**
     * 制品坐标
     */
    private ArtifactCoordinates artifactCoordinates;
    /**
     * 标签集
     */
    private List<TagSet> tagSet;
    /**
     * 文件大小
     */
    private Long sizeInBytes;
    /**
     * 下载次数
     */
    private Integer downloadCount = 0;
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
}
