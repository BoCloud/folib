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
package com.folib.config;

import com.folib.users.UsersFileManager;
import com.folib.users.dto.UsersDto;
import com.folib.users.service.impl.YamlUserService;
import com.folib.users.service.impl.YamlUserService.Yaml;

import java.io.IOException;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Spring configuration for all user-related code.
 *
 * @author 
 * @author veadan
 */
@Configuration
@ComponentScan({ "com.folib.users" })
@Import({ DataServiceConfig.class,
          CommonConfig.class,
          UsersAuthorizationConfig.class,
          UsersSecurityConfig.class })
public class UsersConfig
{

    @Inject
    @Yaml
    private YamlUserService userService;

    @Inject
    private UsersFileManager usersFileManager;

    @PostConstruct
    void init() throws IOException
    {
        final UsersDto securityUsers = usersFileManager.read();
        userService.setUsers(securityUsers);
    }


}
