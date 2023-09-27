package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactStatistics {

    /**
     * 制品总数
     */
    private Long artifactsCount;

    /**
     * 问题制品数
     */
    private Long artifactsVulnerabilitiesCount;

    /**
     * 正常制品数
     */
    private Long artifactsNormalCount;

    /**
     * 漏洞数量
     */
    private Long vulnerabilitiesCount;

    /**
     * 严重的漏洞数量
     */
    private Long criticalVulnerabilitiesCount;

    /**
     * 高危的漏洞数量
     */
    private Long highVulnerabilitiesCount;

    /**
     * 中危的漏洞数量
     */
    private Long mediumVulnerabilitiesCount;

    /**
     * 低危的漏洞数量
     */
    private Long lowVulnerabilitiesCount;

    /**
     * 被封存的漏洞数量
     */
    private Long suppressedVulnerabilitiesCount;
}
