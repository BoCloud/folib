package com.veadan.folib.scanner.entity;


import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author veadan
 * @date 2022/9/9
 **/
@Data
@Accessors(chain = true)
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FolibScannerDockerTableVO implements Serializable {

    /**
     * 制品路径
     */
    private String path;

    /**
     * 版本路径
     */
    private String versionPath;

    /**
     * 版本
     */
    private String version;

    /**
     * 仓库名称
     */
    private String repository;

    /**
     * 存储空间
     */
    private String storage;

    /**
     * 报告
     */
    private String report;

    /**
     * 文件类型
     */
    private String fileType;

    /**
     * 扫描状态
     */
    private String scanStatus;

    /**
     * 是否扫描
     */
    private Boolean onScan;

    /**
     * 扫描时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    private Date scanTime;

    /**
     * 安全级别
     */
    private String level;

    /**
     * 依赖数量
     */
    private Integer dependencyCount;

    /**
     * 容易被攻击的依赖数量
     */
    private Integer vulnerableCount;

    /**
     * 漏洞数量
     */
    private Integer vulnerabilitesCount;

    /**
     * 被封存的数量
     */
    private Integer suppressedCount;

    /**
     *
     */
    @Column(name = "cve_checked_time")
    private Date cveCheckedTime;

    /**
     *
     */
    private Date cveUpdateTime;

    /**
     * 子表数据
     */
    private List<FolibScanner> childList;
}
