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

import com.folib.entity.*;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
  * @Description: 用户权限同步请求参数
  * @auther: fengmg
  * @CreateDate: 2024/8/8 14:15
  * @Version: 1.0
  */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserAuthReq {

    @ApiModelProperty("用户信息")
    protected List<FolibUser> users;
    @ApiModelProperty("角色信息")
    protected List<FolibRole> roles;
    @ApiModelProperty("用户组")
    protected List<UserGroup> groups;
    @ApiModelProperty("资源")
    protected List<Resource> resources;
    @ApiModelProperty("用户组信息")
    protected List<UserGroupRef> userGroups;
    @ApiModelProperty("权限信息")
    protected List<RoleResourceRef> userRoles;

    //TODO 存储空间、仓库

    protected boolean nextPage;
}
