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
package com.folib.forms.users.auth;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.validation.UniqueRoleName;
import lombok.Data;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Veadan
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleForm
{

    @NotBlank(groups = {NewRole.class, UpdateRole.class}, message = "A name must be specified.")
    @UniqueRoleName(groups = {NewRole.class}, message = "Role is already registered.")
    private String name;

    private String description;

    @Valid
    private AccessModelForm privileges;

    private List<AccessResources> resources;

    private List<String> access = new ArrayList<>();
    public interface NewRole {

    }
    public interface UpdateRole {

    }
}
