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
package com.folib.forms.customlayout;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("CustomLayoutForm")
public class CustomLayoutForm implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private String id;

    /**
     * 布局名称
     */
    @ApiModelProperty("布局名称")
    @NotBlank(message = "请填写布局名称", groups = {SaveGroup.class, UpdateGroup.class, DeleteGroup.class})
    private String layoutName;

    /**
     * 模糊布局名称
     */
    private String matchLayoutName;

    /**
     * 制品路径正则表达式
     */
    @ApiModelProperty("制品路径正则表达式")
    @NotBlank(message = "请填写制品路径正则表达式", groups = {SaveGroup.class, UpdateGroup.class})
    private String artifactPathPattern;

    public interface SaveGroup
            extends Serializable {
        // 新增组
    }

    public interface UpdateGroup
            extends Serializable {
        // 更新组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }
}
