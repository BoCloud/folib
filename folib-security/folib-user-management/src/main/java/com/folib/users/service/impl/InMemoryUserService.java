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

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.data.CacheName;
import com.folib.domain.PageResultResponse;
import com.folib.domain.SecurityRole;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.JwtAuthenticationClaimsProvider;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.UserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.users.userdetails.UserDetailsMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.folib.domain.User;
import com.folib.users.domain.UserData;
import com.folib.users.domain.Users;
import com.folib.users.dto.UserDto;
import com.folib.users.dto.UsersDto;
import com.folib.util.LocalDateTimeInstance;

import org.jose4j.lang.JoseException;
import org.springframework.cache.annotation.CacheEvict;


public class InMemoryUserService implements UserService
{

    protected Map<String, UserDto> userMap = new ConcurrentHashMap<>();

    private final ReadWriteLock usersLock = new ReentrantReadWriteLock();

    @Inject
    private SecurityTokenProvider tokenProvider;

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Override
    public Users getUsers()
    {
        final Lock readLock = usersLock.readLock();
        readLock.lock();

        try
        {
            Set<UserDto> userSet = new HashSet<>(userMap.values());

            return new Users(new UsersDto(userSet));
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public PageResultResponse<User> queryUser(User user, Integer page, Integer limit) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        int index = (page - 1);
        final Lock readLock = usersLock.readLock();
        readLock.lock();
        try
        {
            Set<UserDto> userSet = new HashSet<>(userMap.values());
            if (CollectionUtils.isNotEmpty(userSet)) {
                List<UserDto> userList = Lists.newLinkedList(userSet);
                List<List<UserDto>> userLists = Lists.partition(userList, limit);
                if (userLists.size() >= page) {
                    return new PageResultResponse<User>(userSet.size(), userLists.get(index).stream().map(UserData::new).collect(Collectors.toList()));
                } else {
                    return null;
                }
            }
            return null;
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public User findByUsername(final String username)
    {
        if (username == null)
        {
            return null;
        }

        final Lock readLock = usersLock.readLock();
        readLock.lock();

        try
        {
            return Optional.ofNullable(userMap.get(username))
                           .map(UserData::new)
                           .orElse(null);
        }
        finally
        {
            readLock.unlock();
        }
    }

    @Override
    public String generateSecurityToken(final String username)
            throws JoseException
    {
        return generateSecurityToken(username, null);
    }

    @Override
    public String generateSecurityToken(String username, Integer expireSeconds) throws JoseException {
        final User user = findByUsername(username);

        if (StringUtils.isEmpty(user.getSecurityTokenKey()))
        {
            return null;
        }

        SpringSecurityUser springSecurityUser = userDetailsMapper.apply(user);
        Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
        return tokenProvider.getToken(username, claimMap, expireSeconds, null);
    }

    @Override
    public void revokeEveryone(final String roleToRevoke)
    {
        modifyInLock(users -> {
            users.values().forEach(user -> user.removeRole(roleToRevoke));
        });
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public User save(final User user)
    {
        return modifyInLock(users -> {
            UserDto userDto = Optional.ofNullable(users.get(user.getUsername())).orElseGet(() -> new UserDto());

            if (!StringUtils.isBlank(user.getPassword()))
            {
                userDto.setPassword(user.getPassword());
            }

            userDto.setUsername(user.getUsername());
            userDto.setEnabled(user.isEnabled());
            List<String> showRoleNameList = Lists.newArrayList(SystemRole.ADMIN.name(), SystemRole.ARTIFACTS_MANAGER.name(), SystemRole.GENERAL.name(), SystemRole.OPEN_SOURCE_MANAGE.name());
            Set<SecurityRole> roles = Optional.ofNullable(userDto.getRoles()).orElse(Sets.newLinkedHashSet()).stream().filter(item -> !showRoleNameList.contains(item.getRoleName())).collect(Collectors.toSet());
            roles.addAll(user.getRoles());
            userDto.setRoles(roles);
            userDto.setSecurityTokenKey(user.getSecurityTokenKey());
            userDto.setLastUpdate(LocalDateTimeInstance.now());

            users.putIfAbsent(user.getUsername(), userDto);

            return userDto;
        });
    }

    @Override
    public User saveOverrideRole(User user) {
        return modifyInLock(users -> {
            UserDto userDto = Optional.ofNullable(users.get(user.getUsername())).orElseGet(() -> new UserDto());

            if (!StringUtils.isBlank(user.getPassword()))
            {
                userDto.setPassword(user.getPassword());
            }

            userDto.setUsername(user.getUsername());
            userDto.setEnabled(user.isEnabled());
            userDto.setRoles(user.getRoles());
            userDto.setSecurityTokenKey(user.getSecurityTokenKey());
            userDto.setLastUpdate(LocalDateTimeInstance.now());

            users.putIfAbsent(user.getUsername(), userDto);

            return userDto;
        });
    }

    @Override
    public void deleteByUsername(final String username)
    {
        modifyInLock(users -> {
            users.remove(username);
        });
    }

    @Override
    public List<User> findUserByRoles(List<String> rolesList) {
        if (rolesList == null)
        {
            return null;
        }

        final Lock readLock = usersLock.readLock();
        readLock.lock();

        try
        {
            List<UserDto> users = userMap.values().stream().filter(user -> user.getRoles().stream().anyMatch(role -> rolesList.contains(role.getRoleName()))).collect(Collectors.toList());
            return Optional.ofNullable(users).orElse(Collections.emptyList()).stream()
                    .map(UserData::new).collect(Collectors.toList());
        }
        finally
        {
            readLock.unlock();
        }
    }
    @Override
    public void updateAccountDetailsByUsername(User userToUpdate)
    {
        modifyInLock(users -> {
            Optional.ofNullable(users.get(userToUpdate.getUsername()))
                    .ifPresent(user -> {
                        if (!StringUtils.isBlank(userToUpdate.getPassword()))
                        {
                            user.setPassword(userToUpdate.getPassword());
                        }
                        if (!StringUtils.isBlank(userToUpdate.getOriginalPassword()))
                        {
                            user.setOriginalPassword(userToUpdate.getOriginalPassword());
                        }
                        if (!StringUtils.isBlank(userToUpdate.getAvatar()))
                        {
                            user.setAvatar(userToUpdate.getAvatar());
                        }
                        if (!StringUtils.isBlank(userToUpdate.getEmail()))
                        {
                            user.setEmail(userToUpdate.getEmail());
                        }
                        updateSecurityToken(user, userToUpdate.getSecurityTokenKey());
                    });
        });
    }

    private void updateSecurityToken(final UserDto user,
                                     final String securityToken)
    {
        if (StringUtils.isNotBlank(securityToken))
        {
            user.setSecurityTokenKey(securityToken);
        }
    }

    protected void modifyInLock(final Consumer<Map<String, UserDto>> operation)
    {
        final Lock writeLock = usersLock.writeLock();
        writeLock.lock();

        try
        {
            operation.accept(userMap);
        }
        finally
        {
            writeLock.unlock();
        }
    }

    protected <T> T modifyInLock(final Function<Map<String, UserDto>, T> operation)
    {
        final Lock writeLock = usersLock.writeLock();
        writeLock.lock();

        try
        {
            return operation.apply(userMap);
        }
        finally
        {
            writeLock.unlock();
        }
    }

}
