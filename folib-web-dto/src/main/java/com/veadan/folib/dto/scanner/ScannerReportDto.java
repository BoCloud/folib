package com.veadan.folib.dto.scanner;

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
public class ScannerReportDto {

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
     * 扫描时间
     */
    private String scanTime;
}
