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
package com.folib.forms.scanner;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

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
