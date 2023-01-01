package com.veadan.folib.forms.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Set;

/**
 * @author leipenghui
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RepositoryForm {

    /**
     * uuid
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
     * 镜像名称
     */
    private String imageName;

    /**
     * 镜像版本
     */
    private String version;

    /**
     * 文件路径
     */
    private String filePath;

    /**
     * 文件路径
     */
    private List<ScannerReportForm> filePaths;

    /**
     * 漏洞总数
     */
    private Integer vulnerabilitiesCount;

    /**
     * 依赖数量
     */
    private Integer dependencyCount;

    /**
     * 封存漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount;

    /**
     * 具有漏洞的包数量
     */
    private Integer dependencyVulnerabilitiesCount;

    /**
     * 扫描时间
     */
    private String scanTime;
}
