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
package com.folib.authorization.dto;

import java.io.Serializable;

import com.folib.users.domain.Privileges;
import com.folib.users.dto.AccessModelDto;

/**
 * @author veadan
 */
public class RoleDto
        implements Serializable, Role
{

    private String name;

    private String description;

    private AccessModelDto accessModel;


    public RoleDto()
    {
    }

    public RoleDto(String name,
                   String description,
                   AccessModelDto accessModel)
    {
        this.name = name;
        this.description = description;
        this.accessModel = accessModel;
    }

    @Override
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    @Override
    public String getDescription()
    {
        return description;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

    @Override
    public AccessModelDto getAccessModel()
    {
        return accessModel;
    }

    public void setAccessModel(AccessModelDto accessModel)
    {
        this.accessModel = accessModel;
    }

    public void addPrivilege(Privileges p)
    {
        accessModel.getApiAuthorities().add(p);
    }
    
    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("\n\t\tRole{");
        sb.append("name='").append(name).append('\'');
        sb.append(", description='").append(description);
        sb.append('}');

        return sb.toString();
    }

}
