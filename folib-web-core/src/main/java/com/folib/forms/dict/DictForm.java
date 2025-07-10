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
package com.folib.forms.dict;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;


/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ApiModel("dict")
public class DictForm implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Long id;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

    /**
     * 字典类型
     */
    @ApiModelProperty("字典类型")
    private String dictType;

    /**
     * 字典key
     */
    @ApiModelProperty("字典key")
    private String dictKey;

    /**
     * 字典值
     */
    @ApiModelProperty("字典值")
    private String dictValue;

    /**
     * 别名
     */
    @ApiModelProperty("别名")
    private String alias;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String comment;

    /**
     * 是否覆盖系统属性
     */
    @ApiModelProperty("是否覆盖系统属性 true 覆盖")
    private Boolean overrideSystemProperty;
}
