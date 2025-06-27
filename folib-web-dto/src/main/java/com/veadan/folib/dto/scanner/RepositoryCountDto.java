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
public class RepositoryCountDto {

    /**
     * 存储空间
     */
    private String storage;
    /**
     * 仓库
     */
    private String repository;
    /**
     * 布局类型
     */
    private String layout;
    /**
     * 评分
     */
    private Integer star;
    /**
     * 包数量
     */
    private Long scanCount;
    /**
     * 依赖数量
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
