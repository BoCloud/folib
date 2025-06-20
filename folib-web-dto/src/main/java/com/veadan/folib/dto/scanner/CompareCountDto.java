package com.veadan.folib.dto.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CompareCountDto {

    /**
     * 扫描包总数
     */
    private Long scanCount;

    /**
     * 扫描依赖数量
     */
    private Long dependencyCount;

    /**
     * 具有漏洞的包数量
     */
    private Long dependencyVulnerabilitiesCount;

    /**
     * 漏洞总数
     */
    private Long vulnerabilitiesCount;

    /**
     * 封存漏洞数量
     */
    private Long suppressedVulnerabilitiesCount;
}
