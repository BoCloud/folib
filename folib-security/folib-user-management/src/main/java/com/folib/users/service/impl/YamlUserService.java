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
package com.folib.users.service.impl;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.io.IOException;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.HashSet;
import java.util.Map;
import java.util.function.Consumer;

import javax.inject.Inject;
import javax.inject.Qualifier;

import com.folib.users.UsersFileManager;
import com.folib.users.dto.UserDto;
import com.folib.users.dto.UsersDto;
import com.folib.users.service.impl.YamlUserService.Yaml;
import org.springframework.stereotype.Service;

/**
 * @author 
 * @author veadan
 */
@Service
@Yaml
public class YamlUserService
        extends InMemoryUserService
{

    @Inject
    private UsersFileManager usersFileManager;

    @Override
    protected void modifyInLock(Consumer<Map<String, UserDto>> operation)
    {
        super.modifyInLock(operation.andThen(u -> doStoreUsers()));
    }

    private void doStoreUsers()
    {
        try
        {
            usersFileManager.store(new UsersDto(new HashSet<>(userMap.values())));
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
    }

    public void setUsers(final UsersDto newUsers)
    {
        modifyInLock(users -> {
            users.clear();
            newUsers.getUsers().stream().forEach(u -> users.put(u.getUsername(), u));
        });
    }

    @Documented
    @Retention(RUNTIME)
    @Qualifier
    public @interface Yaml
    {
    }
}
