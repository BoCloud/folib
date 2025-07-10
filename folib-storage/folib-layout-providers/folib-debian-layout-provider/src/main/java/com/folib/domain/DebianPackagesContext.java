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
package com.folib.domain;

import com.folib.constant.DebianConstant;
import com.folib.util.DebianUtils;
import lombok.Data;

/**
 * @author veadan
 * @since 2024-09-02 16:28
 */
@Data
public class DebianPackagesContext {

    private final String distribution;
    private final String component;
    private final String architecture;
    private final String binaryPath;

    private final boolean automaticLayout;

    public DebianPackagesContext(String distribution, String component, String architecture) {
        this.distribution = distribution;
        this.component = component;
        this.architecture = architecture;
        this.automaticLayout = DebianUtils.allAreNotBlank(distribution, component, architecture);
        if (!this.automaticLayout && !DebianUtils.allAreBlank(distribution, component, architecture)) {
            throw new IllegalArgumentException(String.format("All Debian coordinates must be specified: %s/%s/%s", distribution, component, architecture));
        } else {
            this.binaryPath = this.calcBinaryPath();
        }
    }



    private String calcBinaryPath() {
        return this.automaticLayout ? String.format("%s/%s/%s/binary-%s", DebianConstant.PACKAGE_PREFIX, this.distribution, this.component, this.architecture) : "";
    }
}
