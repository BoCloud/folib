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

import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.RoleDto;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class AuthorizationConfig
{

    private final Set<RoleData> roles;

    private final Set<Client> clients;

    public AuthorizationConfig(final AuthorizationConfigDto source)
    {
        this.roles = immuteRoles(source.getRoles());
        this.clients= immuteClients(source.getClients());

    }



    private Set<RoleData> immuteRoles(final Set<RoleDto> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream().map(RoleData::new).collect(
                Collectors.toList())) : Collections.emptySet();
    }

    private Set<Client> immuteClients(final Set<Client> source)
    {
        return source != null ? ImmutableSet.copyOf(source.stream().map(Client::new).collect(
                Collectors.toList())) : Collections.emptySet();
    }



    public Set<RoleData> getRoles()
    {
        return roles;
    }

    public Set<Client> getClients()
    {
        return clients;
    }
}

