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
package com.folib.users.security;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.authorization.AuthorizationConfigFileManager;
import com.folib.authorization.domain.RoleData;
import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.Role;
import com.folib.authorization.dto.RoleDto;
import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.components.DistributedCacheComponent;
import com.folib.users.domain.SystemRole;
import com.folib.users.dto.AccessModelDto;
import jakarta.annotation.PostConstruct;
import jakarta.inject.Inject;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;


import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
@DependsOn("liquibase")
public class AuthoritiesProvider
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesProvider.class);

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;
    @Inject
    private DistributedCacheComponent distributedCacheComponent;
    @PostConstruct
    void init() throws IOException
    {
        final AuthorizationConfigDto config = authorizationConfigFileManager.read();
        authorizationConfigService.setAuthorizationConfig(config);
    }

    public Set<RoleData> getAssignableRoles()
    {
        return authorizationConfigService.get().getRoles();
    }
    
    public Role getRuntimeRole(String name)
    {
        RoleData role = new RoleData(new RoleDto(name, "", new AccessModelDto()));
        Set<RoleData> roles = authorizationConfigService.getRole(name).getRoles();
        if (CollectionUtils.isNotEmpty(roles)) {
            role = roles.stream().findFirst().orElseThrow(() -> new IllegalArgumentException(name));
        }

        if (SystemRole.ADMIN.name().equals(name))
        {
            RuntimeRole adminRole = new RuntimeRole(role, (a) -> new AdminAccessModel());
            return new RuntimeRole(adminRole, AuthenticatedAccessModel::new);
        }
        else if (SystemRole.ANONYMOUS.name().equals(name))
        {
            return new RuntimeRole(role, AnonymousAccessModel::new);
        }

        return new RuntimeRole(role, AuthenticatedAccessModel::new);
    }

    public Set<Role> getRuntimeRole(String roleId, String username)
    {
        String roleKey = String.format("user_role_%s", username);
        String roleStr = distributedCacheComponent.get(roleKey);
        Set<RoleData> roles;
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            if (StrUtil.isEmpty(roleStr)) {
                roles = authorizationConfigService.get(username)
                        .getRoles();

                distributedCacheComponent.put(roleKey, objectMapper.writeValueAsString(roles), 30, TimeUnit.MINUTES);
            } else {
                List<RoleDto> roleDtos = objectMapper.readValue(roleStr, objectMapper.getTypeFactory().constructCollectionType(List.class, RoleDto.class));
                roles = roleDtos.stream().map(RoleData::new).collect(Collectors.toSet());
            }
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }

        Set<Role> roleSet = new HashSet<>();
        return roles.stream().map(r -> {
            if (SystemRole.ADMIN.name().equals(r.getName())) {
                RuntimeRole adminRole = new RuntimeRole(r, (a) -> new AdminAccessModel());
                roleSet.add(new RuntimeRole(adminRole, AuthenticatedAccessModel::new));
            }else if (SystemRole.ANONYMOUS.name().equals(r.getName())) {
                roleSet.add(new RuntimeRole(r, AnonymousAccessModel::new));
            }else {
                roleSet.add(new RuntimeRole(r, AuthenticatedAccessModel::new));
            }
            return roleSet;
        }).flatMap(Collection::stream).collect(Collectors.toSet());

    }

}
