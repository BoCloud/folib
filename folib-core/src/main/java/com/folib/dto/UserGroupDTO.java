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
package com.folib.dto;

import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
* 用户组;
* @author veadan
* @date : 2024-7-17
*/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户组",description = "")
public class UserGroupDTO implements Serializable,Cloneable {
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

     @JsonIgnore
    private String users;

    public List<String> getUserIds() {
        if (StringUtils.isEmpty(users)) {
            return Collections.emptyList();
        }
        return Arrays.asList(users.split(","));
    }

    public List<String> getRoleIds() {
        if (StringUtils.isEmpty(roles)){
           return Collections.emptyList();
        }
        return Arrays.asList(roles.split(","));
    }

    private List<String> userIds;
    @JsonIgnore
    private String roles;
    private List<String> roleIds;

}