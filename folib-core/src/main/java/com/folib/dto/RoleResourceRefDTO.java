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

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

/**
* 权限表;
* @author veadan
* @date : 2024-7-17
*/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class RoleResourceRefDTO implements Serializable,Cloneable {

    /**
     * 角色id
     */
    @ApiModelProperty(name = "角色id", notes = "")
    private String roleId;
    /**
     * 对象id
     */
    @ApiModelProperty(name = "对象id", notes = "")
    private String entityId;
    /**
     * 关联类型[用户、用户组];[1-用户id、2-用户组id]
     */
    @ApiModelProperty(name = "关联类型[用户、用户组]", notes = "[1-用户id、2-用户组id]")
    private String refType;
    /**
     * 资源id
     */
    @ApiModelProperty(name = "资源id", notes = "")
    private Long resourceId;
    /**
     * 资源类型;[1-api、2-存储空间、3-仓库、4-路径]
     */
    @ApiModelProperty(name = "资源类型", notes = "")
    private String resourceType;
    /**
     * 存储空间权限
     */
    @ApiModelProperty(name = "存储空间权限", notes = "")
    private List<String> storagePrivileges;
    /**
     * 仓库权限
     */
    @ApiModelProperty(name = "仓库权限", notes = "")
    private List<String> repositoryPrivileges;
    /**
     * 路径权限
     */
    @ApiModelProperty(name = "路径权限", notes = "")
    private List<String> pathPrivileges;

    public void setStoragePrivileges(String storagePrivileges) {
        if (storagePrivileges != null) {
            this.storagePrivileges = Arrays.asList(storagePrivileges.split(","));
        }
    }

    public void setRepositoryPrivileges(String repositoryPrivileges) {
        if (repositoryPrivileges != null) {
            this.repositoryPrivileges = Arrays.asList(repositoryPrivileges.split(","));
        }
    }

    public void setPathPrivileges(String pathPrivileges) {
        if (pathPrivileges != null) {
            this.pathPrivileges = Arrays.asList(pathPrivileges.split(","));
        }
    }
}