package com.veadan.folib.dto.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CountDto {

    /**
     * 扫描包总数
     */
    private Long scanCount;

    /**
     * 无需扫描包总数
     */
    private Long notScanCount;

    /**
     * 扫描成功的包数量
     */
    private Long scanSuccessCount;

    /**
     * 未扫描数量
     */
    private Long unScanCount;

    /**
     * 扫描失败的包数量
     */
    private Long scanFailCount;

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
