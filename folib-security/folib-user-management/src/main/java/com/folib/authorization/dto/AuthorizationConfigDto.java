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
import java.util.LinkedHashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonRootName;
import com.folib.authorization.domain.Client;

/**
 * Java representation for authorization config that stored in YAML file.
 *
 * @author 
 * @author Veadan
 * @see /src/main/resources/etc/conf/folib-authorization.yaml
 * @see {@linkplain https://dev.carlspring.org/youtrack/issue/SB-126}
 */
@JsonRootName("authorizationConfiguration")
public class AuthorizationConfigDto
        implements Serializable
{

    private Set<RoleDto> roles = new LinkedHashSet<>();

    private  Set<Client> clients=new LinkedHashSet<>();

    public Set<RoleDto> getRoles()
    {
        return roles;
    }



    public void setRoles(Set<RoleDto> roles)
    {
        this.roles = roles;
    }

    public Set<Client> getClients() {
        return clients;
    }

    public void setClients(Set<Client> clients) {
        this.clients = clients;
    }

    @Override
    public boolean equals(final Object o)
    {
        if (this == o) return true;
        if (!(o instanceof AuthorizationConfigDto))
        {
            return false;
        }
        final AuthorizationConfigDto config = (AuthorizationConfigDto) o;
        return java.util.Objects.equals(roles, config.roles);
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("AuthorizationConfig{");
        sb.append("roles=").append(roles);
        sb.append('}');
        return sb.toString();
    }
}
