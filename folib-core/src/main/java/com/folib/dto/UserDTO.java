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

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.folib.users.domain.User is annotated with it
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO
        implements Serializable
{

    private String username;
    private String password;

    private boolean enabled;

    private String email;

    private String avatar;
    private String sourceId;

    private Set<String> userGroups;
    private Set<String> userGroupIds;

    private Set<String> roles;
    private String storageId;
    private String repositoryId;
    private String path;

    @ApiModelProperty(name = "主键",notes = "")
    private String id ;
    @ApiModelProperty(name = "原始密码",notes = "")
    private String originalPassword ;
    @ApiModelProperty(name = "用户类型",notes = "")
    private String userType ;
    @ApiModelProperty(name = "是否删除",notes = "")
    private String deleted ;

    private Date updateTime;

    private String nickname;

    public void setRepositoryPrivilege(String repositoryPrivilege) {
        this.repositoryPrivilege =  new HashSet<>(Arrays.asList(repositoryPrivilege.split(",")));;
    }

    public void setStoragePrivilege(String storagePrivilege) {
        this.storagePrivilege = new HashSet<>(Arrays.asList(storagePrivilege.split(",")));
    }

    public void setPathPrivilege(String pathPrivilege) {
        this.pathPrivilege = new HashSet<>(Arrays.asList(pathPrivilege.split(",")));
    }

    private Set<String> storagePrivilege;
    private Set<String> repositoryPrivilege;
    private Set<String> pathPrivilege;
    private String securityTokenKey;

    private Set<String> authorities;
    public void setUserGroups(String userGroups) {
        if (userGroups != null) {
            this.userGroups = new HashSet<>(Arrays.asList(userGroups.split(",")));
        }
    }
    public void setRoles(String roles) {
        if (roles != null) {
            this.roles = new HashSet<>(Arrays.asList(roles.split(",")));
        }
    }

    public void setUserGroupIds(String userGroupIds) {
        if (userGroupIds != null) {
            this.userGroupIds = new HashSet<>(Arrays.asList(userGroupIds.split(",")));
        }
    }

}
