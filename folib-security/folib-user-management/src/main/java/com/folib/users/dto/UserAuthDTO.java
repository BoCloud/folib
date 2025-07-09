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
package com.folib.users.dto;

import com.folib.entity.*;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.RepositoryDto;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

/**
 * @author veadan
 * @Date: 2024/8/10 08:57
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserAuthDTO{

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

    protected boolean nextPage;

    /**存储空间*/
    private List<StorageDto> storages;
    /**仓库*/
    private List<RepositoryDto> repositorys;

    @ApiModelProperty("删除用户信息")
    protected List<String> removeUserIds;
    @ApiModelProperty("删除角色信息")
    protected List<String> removeRoleIds;
    @ApiModelProperty("删除用户组")
    protected List<Long> removeGroupIds;
    @ApiModelProperty("删除资源")
    protected List<String> removeResourceIds;

    public List<UserGroupRef> getUserGroups() {
        if (CollectionUtils.isNotEmpty(userGroups)) {
            userGroups.forEach(userRole -> userRole.setId(null));
        }
        return userGroups;
    }

    public List<RoleResourceRef> getUserRoles() {
        if (CollectionUtils.isNotEmpty(userRoles)) {
            userRoles.forEach(userRole -> userRole.setId(null));
        }
        return userRoles;
    }
}
