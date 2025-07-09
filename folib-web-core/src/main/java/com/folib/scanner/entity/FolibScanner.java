/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.scanner.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;


/**
 * @author Veadan
 * @email xuxinping@126.com
 * @date 2022-05-31 23:12:54
 */
@Data
@Accessors(chain = true)
@Table(name = "folib_scanner")
@ApiModel("folib_scanner")
public class FolibScanner implements Serializable {
    private static final long serialVersionUID = 1L;


    @Id
    @ApiModelProperty("制品路径")
    @Column(name = "path")
    private String path;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @Column(name = "repository")
    private String repository;

    /**
     * 存储空间
     */
    @ApiModelProperty("存储空间")
    @Column(name = "storage")
    private String storage;

    /**
     * 制品路径
     */
    @ApiModelProperty("制品路径")
    @Column(name = "artifact_path")
    private String artifactPath;

    /**
     * 报告
     */
    @ApiModelProperty("报告")
    @Column(name = "report")
    private String report;

    /**
     * 文件类型
     */
    @ApiModelProperty("文件类型")
    @Column(name = "file_type")
    private String fileType;

    /**
     * 扫描状态
     */
    @ApiModelProperty("扫描状态")
    @Column(name = "scan_status")
    private String scanStatus;

    /**
     * 是否扫描
     */
    @ApiModelProperty("是否扫描")
    @Column(name = "on_scan")
    private Boolean onScan;

    /**
     * 扫描时间
     */
    @ApiModelProperty("扫描时间")
    @Column(name = "scan_time")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+8")
    private Date scanTime;

    /**
     * 安全级别
     */
    @ApiModelProperty("安全级别")
    @Column(name = "level")
    private String level;

    /**
     * 依赖数量
     */
    @ApiModelProperty("依赖数量")
    @Column(name = "dependency_count")
    private Integer dependencyCount;

    /**
     * 容易被攻击的依赖数量
     */
    @ApiModelProperty("容易被攻击的依赖数量")
    @Column(name = "vulnerable_count")
    private Integer vulnerableCount;

    /**
     * 漏洞数量
     */
    @ApiModelProperty("漏洞数量")
    @Column(name = "vulnerabilites_count")
    private Integer vulnerabilitesCount;

    /**
     * 被封存的数量
     */
    @ApiModelProperty("被封存的数量")
    @Column(name = "suppressed_count")
    private Integer suppressedCount;

    /**
     *
     */
    @ApiModelProperty("")
    @Column(name = "cve_checked_time")
    private Date cveCheckedTime;

    /**
     *
     */
    @ApiModelProperty("")
    @Column(name = "cve_update_time")
    private Date cveUpdateTime;


}
