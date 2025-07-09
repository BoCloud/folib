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
package com.folib.authorization.domain;

import javax.annotation.concurrent.Immutable;

import com.folib.authorization.dto.Role;
import com.folib.authorization.dto.RoleDto;
import com.folib.users.domain.AccessModelData;
import com.folib.users.dto.AccessModel;
import com.folib.users.dto.AccessModelDto;

/**
 * @author veadan
 */
@Immutable
public class RoleData implements Role
{

    private final String name;

    private final String description;

    private final AccessModel accessModel;

    public RoleData(final RoleDto source)
    {
        this.name = source.getName();
        this.description = source.getDescription();
        this.accessModel = immuteAccessModel(source.getAccessModel());
    }

    private AccessModel immuteAccessModel(AccessModelDto source)
    {
        return source != null ? new AccessModelData(source) : null;
    }

    public String getName()
    {
        return name;
    }

    public String getDescription()
    {
        return description;
    }

    public AccessModel getAccessModel()
    {
        return accessModel;
    }

}
