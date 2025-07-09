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
package com.folib.controllers.users.support;

import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

import java.util.stream.Collectors;
import com.folib.domain.User;
import com.folib.domain.SecurityRole;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.collect.ImmutableSet;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.folib.users.domain.User is annotated with it
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserOutput
        implements Serializable
{

    private String username;

    private boolean enabled;

    private String email;

    private String avatar;

    private String nickname;

    private Set<String> roles;

    private String securityTokenKey;

    private LinkedHashSet<String> authorities;

    private Set<String> userGroups;
    private Set<String> userGroupIds;

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(boolean enabled)
    {
        this.enabled = enabled;
    }

    public Set<String> getRoles()
    {
        return roles == null ? Collections.emptySet() : ImmutableSet.copyOf(roles);
    }

    public void setRoles(Set<String> roles)
    {
        this.roles = roles;
    }

    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey)
    {
        this.securityTokenKey = securityTokenKey;
    }

    public static UserOutput fromUser(User user)
    {
        final UserOutput output = new UserOutput();
        output.setEnabled(user.isEnabled());
        output.setUserGroups(user.getUserGroups());
        output.setUserGroupIds(user.getUserGroupIds());
        output.setRoles(user.getRoles()
                            .stream()
                            .map(SecurityRole::getRoleName)
                            .collect(Collectors.toSet()));
        output.setUsername(user.getUsername());
        output.setEmail(user.getEmail());
        output.setSecurityTokenKey(user.getSecurityTokenKey());
        output.setAvatar(user.getAvatar());
        output.setNickname(user.getNickname());
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.nonNull(authentication)) {
            output.setAuthorities(authentication.getAuthorities().stream()
                    .map(GrantedAuthority::getAuthority)
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)));
        }
        return output;
    }

    public LinkedHashSet<String> getAuthorities() {
        return authorities;
    }

    public void setAuthorities(LinkedHashSet<String> authorities) {
        this.authorities = authorities;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("UserOutput{");
        sb.append("username='").append(username).append('\'');
        sb.append(", enabled=").append(enabled);
        sb.append(", roles=").append(roles);
        sb.append(", authorities=").append(authorities);
        sb.append(", securityTokenKey='").append(securityTokenKey).append('\'');
        sb.append('}');
        return sb.toString();
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public Set<String> getUserGroups() {
        return userGroups;
    }

    public void setUserGroups(Set<String> userGroups) {
        this.userGroups = userGroups;
    }
    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    public void setUserGroupIds(Set<String> userGroupIds) {
        this.userGroupIds = userGroupIds;
    }

    public String getNickname() {
		return nickname;
	}

    public void setNickname(String nickname) {
		this.nickname = nickname;
	}
}
