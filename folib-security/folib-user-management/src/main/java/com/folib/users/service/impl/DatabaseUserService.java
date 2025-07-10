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

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import javax.inject.Inject;
import javax.inject.Qualifier;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.data.CacheName;
import com.folib.domain.PageResultResponse;
import com.folib.domain.SecurityRole;
import com.folib.users.domain.SystemRole;
import com.folib.users.domain.Users;
import com.folib.users.security.JwtAuthenticationClaimsProvider;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.UserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.users.userdetails.UserDetailsMapper;
import jakarta.transaction.Transactional;
import org.apache.commons.lang3.StringUtils;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.repositories.UserRepository;
import com.folib.users.service.impl.DatabaseUserService.Database;
import com.folib.util.LocalDateTimeInstance;

import org.jose4j.lang.JoseException;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@Database
@Transactional
public class DatabaseUserService implements UserService
{

    @Inject
    private SecurityTokenProvider tokenProvider;
    
    @Inject
    protected UserRepository userRepository;

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0")
    public void deleteByUsername(String username)
    {
        userRepository.deleteById(username);
    }

    @Override
    public List<User> findUserByRoles(List<String> rolesList) {
        return userRepository.findUsersWithRoles(rolesList);
    }


    @Override
    public UserEntity findByUsername(String username)
    {
        return userRepository.findById(username).map(UserEntity.class::cast).orElse(null);
    }

    @Override
    public String generateSecurityToken(String username)
        throws JoseException
    {
        return generateSecurityToken(username, null);
    }

    @Override
    public String generateSecurityToken(String username, Integer expireSeconds) throws JoseException {
        final User user = findByUsername(username);
        SpringSecurityUser springSecurityUser = userDetailsMapper.apply(user);
        Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
        return tokenProvider.getToken(username, claimMap, expireSeconds, null);
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public void updateAccountDetailsByUsername(User userToUpdate)
    {
        UserEntity user = findByUsername(userToUpdate.getUsername());
        if (user == null)
        {
            throw new UsernameNotFoundException(userToUpdate.getUsername());
        }

        if (!StringUtils.isBlank(userToUpdate.getPassword()))
        {
            user.setPassword(userToUpdate.getPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getOriginalPassword()))
        {
            user.setOriginalPassword(userToUpdate.getOriginalPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getSecurityTokenKey()))
        {
            user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        }

        if (!StringUtils.isBlank(userToUpdate.getAvatar()))
        {
            user.setAvatar(userToUpdate.getAvatar());
        }

        if (!StringUtils.isBlank(userToUpdate.getEmail()))
        {
            user.setEmail(userToUpdate.getEmail());
        }
        save(user);
    }

    @Override
    public Users getUsers()
    {
        Iterable<User> users = userRepository.findAll();
        return new Users(StreamSupport.stream(users.spliterator(), false).collect(Collectors.toSet()));
    }

    @Override
    public PageResultResponse<User> queryUser(User user, Integer page, Integer limit) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        int start = (page - 1) * limit;
        limit = page * limit;
        long count = userRepository.countUsers(user);
        if (count == 0L) {
            return null;
        }
        List<User> userList = userRepository.findUsersPage(user, start, limit);
        return new PageResultResponse<User>(count, userList);
    }

    @Override
    public void revokeEveryone(String roleToRevoke)
    {
        List<User> resultList = userRepository.findUsersWithRole(roleToRevoke);

        resultList.forEach(user -> {
            user.getRoles().remove(roleToRevoke);
            save(user);
        });
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public User save(User user)
    {
        LocalDateTime now = LocalDateTimeInstance.now();

        UserEntity userEntity = Optional.ofNullable(findByUsername(user.getUsername())).orElseGet(() -> new UserEntity(user.getUsername()));

        if (!StringUtils.isBlank(user.getPassword()))
        {
            userEntity.setPassword(user.getPassword());
        }
        if (StringUtils.isNotBlank(user.getOriginalPassword()))
        {
            userEntity.setOriginalPassword(user.getOriginalPassword());
        }
        userEntity.setEnabled(user.isEnabled());
        List<String> showRoleNameList = Lists.newArrayList(SystemRole.ADMIN.name(), SystemRole.ARTIFACTS_MANAGER.name(), SystemRole.GENERAL.name(), SystemRole.OPEN_SOURCE_MANAGE.name());
        Set<SecurityRole> roles = Optional.ofNullable(userEntity.getRoles()).orElse(Sets.newLinkedHashSet()).stream().filter(item -> !showRoleNameList.contains(item.getRoleName())).collect(Collectors.toSet());
        roles.addAll(user.getRoles());
        userEntity.setRoles(roles);
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        userEntity.setAvatar(user.getAvatar());

//        if (StringUtils.isNotBlank(user.getSourceId()) || StringUtils.isNotBlank(userEntity.getSourceId()))
//        {
//            throw new IllegalStateException("Can't modify external users.");
//        }
        
        return userRepository.save(userEntity);
    }

    @Override
    public User saveOverrideRole(User user) {
        LocalDateTime now = LocalDateTimeInstance.now();

        UserEntity userEntity = Optional.ofNullable(findByUsername(user.getUsername())).orElseGet(() -> new UserEntity(user.getUsername()));

        if (!StringUtils.isBlank(user.getPassword()))
        {
            userEntity.setPassword(user.getPassword());
        }
        userEntity.setEnabled(user.isEnabled());
        userEntity.setRoles(user.getRoles());
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        return userRepository.save(userEntity);
    }

    public void expireUser(String username, boolean clearSourceId)
    {
        UserEntity externalUserEntry = findByUsername(username);
        externalUserEntry.setLastUpdated(LocalDateTime.ofInstant(Instant.ofEpochMilli(0), ZoneId.systemDefault()));

        if (clearSourceId)
        {
            externalUserEntry.setSourceId("empty");
        }

        userRepository.save(externalUserEntry);
    }

    @Documented
    @Retention(RUNTIME)
    @Qualifier
    public @interface Database
    {
    }

}
