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
package com.folib.users.domain;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.concurrent.Immutable;

import com.folib.domain.User;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;
import com.folib.users.dto.UserDto;
import org.springframework.security.core.userdetails.UserDetails;

import com.google.common.collect.ImmutableSet;

/**
 * @author veadan
 */
@Immutable
public class UserData implements Serializable, User
{
    public static final String SECURITY_TOKEN_KEY = "security-token-key";

    private final String username;

    private final String password;

    private final String originalPassword;

    private final String email;

    private String avatar;

    private final String userType;

    private final Boolean enabled;

    private final Set<SecurityRole> roles;

    private Set<Long> groupIds = new HashSet<>();
    private Set<String> userGroups = new HashSet<>();
    private Set<String> userGroupIds = new HashSet<>();
    private final String securityTokenKey;

    private final LocalDateTime lastUpdate;

    private String sourceId;

    private String nickname;

    public UserData(final UserDetails source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.originalPassword = null;
        this.enabled = source.isEnabled();
        this.roles = source.getAuthorities()
                           .stream()
                           .map(a -> new SecurityRoleEntity(a.getAuthority()))
                           .collect(Collectors.toSet());
        this.securityTokenKey = null;
        this.lastUpdate = null;
        this.email=null;
        this.userType=null;
        this.avatar = null;
        this.nickname = null;
    }

    public UserData(final UserDto source)
    {
        this.username = source.getUsername();
        this.password = source.getPassword();
        this.originalPassword = source.getOriginalPassword();
        this.enabled = source.isEnabled();
        this.roles = immuteRoles(source.getRoles());
        this.securityTokenKey = source.getSecurityTokenKey();
        this.lastUpdate = source.getLastUpdated();
        this.sourceId = source.getSourceId();
        this.email=source.getEmail();
        this.userType=source.getUserType();
        this.avatar = source.getAvatar();
        this.nickname = source.getNickname();
    }

    private Set<SecurityRole> immuteRoles(final Set<SecurityRole> source)
    {
        return source != null ? ImmutableSet.copyOf(source) : Collections.emptySet();
    }

    @Override
    public String getUuid()
    {
        return getUsername();
    }

    @Override
    public String getUsername()
    {
        return username;
    }

    @Override
    public String getEmail() {
        return email;
    }

    @Override
    public String getUserType() {
        return userType;
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    @Override
    public String getOriginalPassword()
    {
        return originalPassword;
    }

    @Override
    public Set<SecurityRole> getRoles()
    {
        return roles;
    }

    @Override
    public Set<Long> getGroupIds() {
        return groupIds != null ? new HashSet<>(groupIds)
                : new HashSet<>();

    }

    @Override
    public Set<String> getUserGroups() {
        return userGroups;
    }

    @Override
    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    @Override
    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    @Override
    public Boolean isEnabled()
    {
        return enabled;
    }

    @Override
    public LocalDateTime getLastUpdated()
    {
        return lastUpdate;
    }

    @Override
    public String getSourceId()
    {
        return sourceId;
    }

    @Override
    public String getAvatar() {
        return avatar;
    }

    @Override
    public String getNickname() {
        return this.nickname;
    }

    @Override
    public String toString() {
        return "UserData{" +
                "username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", email='" + email + '\'' +
                ", userType='" + userType + '\'' +
                ", enabled=" + enabled +
                ", roles=" + roles +
                ", securityTokenKey='" + securityTokenKey + '\'' +
                ", lastUpdate=" + lastUpdate +
                ", sourceId='" + sourceId + '\'' +
                '}';
    }
}
