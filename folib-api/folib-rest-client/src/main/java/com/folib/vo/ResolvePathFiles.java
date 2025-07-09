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
package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 * @date 2024/12/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ResolvePathFiles {

    /**
     * glob路径匹配模式
     */
    @NotBlank(message = "请传入pattern参数")
    private String pattern;

    /**
     * 文件下载到本地的位置
     */
    @NotBlank(message = "请传入target参数")
    private String target;

    /**
     * 是否递归子目录，true递归子目录，false不递归子目录，默认为true
     */
    private Boolean recursive = true;

    /**
     * 是否创建子目录，true创建子目录，false不创建子目录，默认为false
     */
    private Boolean flat = false;

}
