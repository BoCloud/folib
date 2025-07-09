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

import com.google.common.base.Objects;
import com.folib.authorization.dto.Role;
import com.folib.users.domain.Privileges;
import com.folib.users.dto.AccessModel;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SpringSecurityUser
        implements UserDetails {

    private String username;

    private String password;

    private String email;

    private String avatar;

    private String userType = "general";

    private Boolean enabled;

    private Set<Role> roles = Collections.emptySet();

    private String url;

    private String securityKey;

    private String sourceId;

    @Override
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public boolean isAccountNonExpired() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public boolean isAccountNonLocked() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return Boolean.TRUE.equals(enabled);
    }

    @Override
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    public boolean isEnabled() {
        return Boolean.TRUE.equals(enabled);
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Set<Role> getRoles() {
        return roles;
    }

    public void setRoles(Set<Role> roles) {
        this.roles = roles;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    @Override
    public Collection<Privileges> getAuthorities() {
        return roles.stream().flatMap(r -> {
            AccessModel accessModel = r.getAccessModel();
            if (accessModel != null) {
                return accessModel.getApiAuthorities().stream();
            } else {
                return Stream.empty();
            }
        }).collect(Collectors.toSet());
    }

    public Collection<Privileges> getStorageAuthorities(String path) {
        return getRoles().stream()
                .flatMap(r -> r.getAccessModel()
                        .getPathAuthorities(path)
                        .stream())
                .collect(Collectors.toSet());
    }

    public Collection<Privileges> getStorageAuthorities(String path, boolean enableSplitPath) {
        return getRoles().stream()
                .flatMap(r -> r.getAccessModel()
                        .getPathAuthorities(path, enableSplitPath)
                        .stream())
                .collect(Collectors.toSet());
    }

    public Collection<Privileges> getStorageAuthorities(String serverName, String storageId, String repositoryId, List<String> paths) {
        Collection<Privileges> privilegesCollection = getRoles().stream()
                .flatMap(r -> r.getAccessModel()
                        .getPathAuthorities(storageId, repositoryId, paths)
                        .stream())
                .collect(Collectors.toSet());
        UserPrivileges.handlerRestrictedRepository(serverName, privilegesCollection, storageId, repositoryId);
        return privilegesCollection;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getSecurityKey() {
        return securityKey;
    }

    public void setSecurityKey(String securityKey) {
        this.securityKey = securityKey;
    }

    public String getSourceId() {
        return sourceId;
    }

    public void setSourceId(String sourceId) {
        this.sourceId = sourceId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SpringSecurityUser user = (SpringSecurityUser) o;
        return enabled.equals(user.enabled) &&
                Objects.equal(username, user.username) &&
//                Objects.equal(password, user.password) &&
//                Objects.equal(roles, user.roles) &&
                Objects.equal(url, user.url) &&
                Objects.equal(securityKey, user.securityKey) &&
                Objects.equal(sourceId, user.sourceId);
    }

    @Override
    public int hashCode() {
        String[] hashCodeTargets = new String[5];
        int i = 0;
        /*String[] hashCodeTargets = new String[roles.size() + 5];
        int i = 0;
        for (Role role : roles) {
            hashCodeTargets[i++] = role.getName();
        }*/
        hashCodeTargets[i++] = String.valueOf(username);
//        hashCodeTargets[i++] = String.valueOf(password);
        hashCodeTargets[i++] = String.valueOf(enabled);
        hashCodeTargets[i++] = String.valueOf(securityKey);
        hashCodeTargets[i++] = String.valueOf(url);
        hashCodeTargets[i++] = String.valueOf(sourceId);

        Arrays.sort(hashCodeTargets);

        return Arrays.hashCode(hashCodeTargets);
    }

    @Override
    public String toString() {
        String role = CollectionUtils.isNotEmpty(roles) ? roles.stream().map(Role::getName).collect(Collectors.joining(",")) : "";
        return "SpringSecurityUser{" +
                "username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", email='" + email + '\'' +
                ", userType='" + userType + '\'' +
                ", enabled=" + enabled +
                ", roles='" + role + '\'' +
                ", url='" + url + '\'' +
                ", securityKey='" + securityKey + '\'' +
                ", sourceId='" + sourceId + '\'' +
                '}';
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }
}
