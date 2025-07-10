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

/**
 * @author leipenghui
 * @date 2022/12/28
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CountForm {

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
