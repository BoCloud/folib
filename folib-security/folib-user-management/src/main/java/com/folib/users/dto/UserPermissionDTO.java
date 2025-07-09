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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Collection;

/**
 * @author Veadan
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserPermissionDTO
{

    @NotEmpty(message = "Username is required!")
    private String userId;

    private Collection<String> privileges;

    @NotNull(message = "roleIds is required!")
    private Collection<String> roleIds;

    public @NotEmpty(message = "Username is required!") String getUserId() {
        return userId;
    }

    public void setUserId(@NotEmpty(message = "Username is required!") String userId) {
        this.userId = userId;
    }

    public Collection<String> getPrivileges() {
        return privileges;
    }

    public void setPrivileges(Collection<String> privileges) {
        this.privileges = privileges;
    }

    public @NotNull(message = "roleIds is required!") Collection<String> getRoleIds() {
        return roleIds;
    }

    public void setRoleIds(@NotNull(message = "roleIds is required!") Collection<String> roleIds) {
        this.roleIds = roleIds;
    }
}
