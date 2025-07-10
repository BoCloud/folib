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
package com.folib.users.userdetails;

import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.users.security.AuthoritiesProvider;

import javax.inject.Inject;

import com.folib.domain.User;

import com.folib.users.service.FolibUserService;
import org.springframework.stereotype.Component;

@Component
public class UserDetailsMapper implements FolibUserToUserDetails
{

    @Inject
    private AuthoritiesProvider authoritiesProvider;
    @Inject
    private FolibUserService folibUserService;
    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Override
    public SpringSecurityUser apply(User user)
    {

        SpringSecurityUser springUser = new SpringSecurityUser();
        springUser.setEnabled(user.isEnabled());
        springUser.setEmail(user.getEmail());
        springUser.setUserType("general");
        springUser.setPassword(getPasswordWithEncodingAlgorithm(user.getPassword()));
        springUser.setUsername(user.getUsername());
        /*springUser.setRoles(user.getRoles()
                                .stream()
                                .map(SecurityRole::getRoleName)
                                .map(authoritiesProvider::getRuntimeRole)
                                .collect(Collectors.toSet()));*/
        springUser.setRoles(authoritiesProvider.getRuntimeRole(null, user.getUsername()));
        springUser.setSecurityKey(user.getSecurityTokenKey());
        springUser.setSourceId(user.getSourceId());

        return springUser;
    }

    private String getPasswordWithEncodingAlgorithm(String password)
    {
        String algorithmPrefix = extractAlgorithmPrefix(password);
        if (algorithmPrefix == null)
        {
            return "{bcrypt}" + password;
        }
        return password;
    }

    private String extractAlgorithmPrefix(String prefixEncodedPassword)
    {
        if (prefixEncodedPassword == null)
        {
            return null;
        }
        int start = prefixEncodedPassword.indexOf("{");
        if (start != 0)
        {
            return null;
        }
        int end = prefixEncodedPassword.indexOf("}", start);
        if (end < 0)
        {
            return null;
        }
        return prefixEncodedPassword.substring(start + 1, end);
    }

}
