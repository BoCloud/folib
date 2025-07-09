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
package com.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.folib.users.domain.User is annotated with it
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserGroupOutput
        implements Serializable
{
    /**
     * 主键
     */
    @ApiModelProperty(name = "主键", notes = "")
    private Long id;
    /**
     * 组名称
     */
    @ApiModelProperty(name = "组名称", notes = "")
    private String groupName;
    /**
     * 描述
     */
    @ApiModelProperty(name = "描述", notes = "")
    private String description;
    /**
     * 新建用户是否自动加入此用户组
     */
    @ApiModelProperty(name = "新建用户是否自动加入此用户组", notes = "")
    private String joinGroup;
    /**
     * 是否删除
     */
    @ApiModelProperty(name = "是否删除", notes = "")
    private String deleted;
    /** 是否默认 */
    @ApiModelProperty(name = "是否默认", notes = "")
    private String isDefault;

    private List<String> userIds;
    private List<String> roleIds;

}
