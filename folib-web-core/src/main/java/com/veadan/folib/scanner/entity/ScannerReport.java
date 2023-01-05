package com.veadan.folib.scanner.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/12/29
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ScannerReport {

    /**
     * 文件路径
     */
    private String filePath;

    /**
     * 漏洞数量
     */
    private Integer vulnerabilitiesCount;

    /**
     * 依赖数量
     */
    private Integer dependencyCount;

    /**
     * 有漏洞的依赖数量
     */
    private Integer dependencyVulnerabilitiesCount;

    /**
     * 被封存的漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount;
    /**
     * 扫描日期
     */
    private String scanDate;
    /**
     * 扫描时间
     */
    private String scanDateTime;
}
