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
package com.folib.users.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import java.util.stream.Collectors;
import com.folib.domain.User;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

/**
 * @author veadan
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserDto
        implements Serializable, User
{

    private String id;
    private String username;
    /**模糊匹配用户名**/
    private String matchUsername;

    private String password;

    private String originalPassword;

    private String email;
    /**模糊匹配用户名**/
    private String matchEmail;

    private String avatar;

    private String userType="general";

    private Boolean enabled = true;

    private Set<String> roles = new HashSet<>();

    private Set<Long> groupIds = new HashSet<>();
    private Set<String> userGroups = new HashSet<>();
    private Set<String> userGroupIds = new HashSet<>();

    private String securityTokenKey;

    private LocalDateTime lastUpdate;

    private String sourceId;

    private String nickname;

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

    public void setEmail(String email) {
        this.email = email;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    @Override
    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    @Override
    @JsonIgnore
    public Set<SecurityRole> getRoles()
    {
        return roles != null ? roles.stream()
                                    .map(role -> new SecurityRoleEntity(role))
                                    .collect(Collectors.toSet())
                             : new HashSet<>();
    }

    public void setRoles(Set<SecurityRole> roles)
    {
        if (roles == null)
        {
            this.roles = new HashSet<>();
            return;
        }
        this.roles = roles.stream().map(SecurityRole::getRoleName).collect(Collectors.toSet());
    }
    @Override
    @JsonIgnore
    public Set<Long> getGroupIds()
    {
        return groupIds;
    }

    public void setGroupIds(Set<Long> groupIds)
    {
        if (groupIds == null)
        {
            this.groupIds = new HashSet<>();
            return;
        }
        this.groupIds = groupIds;
    }

    public void setRoleNames(Set<String> roles)
    {
        this.roles = roles != null ? new HashSet<>(roles) : new HashSet<>();
    }

    @JsonProperty("roles")
    public Set<String> getRoleNames()
    {
        return Collections.unmodifiableSet(roles);
    }

    
    public void addRole(String role)
    {
        roles.add(role);
    }

    public void removeRole(String role)
    {
        removeRole(new SecurityRoleEntity(role));
    }

    public void removeRole(SecurityRole role)
    {
        roles.remove(role);
    }

    public boolean hasRole(SecurityRole role)
    {
        return roles.contains(role);
    }

    @Override
    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey)
    {
        this.securityTokenKey = securityTokenKey;
    }

    @Override
    public Boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(final Boolean enabled)
    {
        this.enabled = enabled;
    }

    @Override
    @JsonIgnore
    public LocalDateTime getLastUpdated()
    {
        return lastUpdate;
    }

    public void setLastUpdate(LocalDateTime lastUpdate)
    {
        this.lastUpdate = lastUpdate;
    }

    @Override
    @JsonIgnore
    public String getSourceId()
    {
        return sourceId;
    }

    public void setSourceId(String sourceId)
    {
        this.sourceId = sourceId;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("User{");
        sb.append("username='")
          .append(username)
          .append('\'');
        sb.append(", roles=")
          .append(roles);
        sb.append(", email=")
                .append(email);
        sb.append('}');
        return sb.toString();
    }

    @Override
    public String getAvatar() {
        return avatar;
    }


    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    @Override
    public String getOriginalPassword() {
        return originalPassword;
    }

    public void setOriginalPassword(String originalPassword) {
        this.originalPassword = originalPassword;
    }
    @Override
    public Set<String> getUserGroups() {
        return userGroups;
    }

    @Override
    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    public void setUserGroups(Set<String> userGroups) {
        this.userGroups = userGroups;
    }

    @Override
    public String getNickname() {
        return  this.nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }
}
