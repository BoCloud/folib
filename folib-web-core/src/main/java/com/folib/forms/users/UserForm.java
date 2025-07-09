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
package com.folib.forms.users;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.ImmutableSet;
import com.folib.validation.users.Password;
import com.folib.validation.users.UniqueUsername;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserForm
        implements Serializable {

    @NotEmpty(groups = {NewUser.class}, message = "Username is required!")
    @UniqueUsername(groups = NewUser.class, message = "Username is already taken.")
    private String username;

    @Password(groups = {NewUser.class}, allowNull = true, min = 12)
    @Password(groups = {ExistingUser.class}, allowNull = true, min = 12)
    private String password;

    @Password(groups = {NewUser.class}, allowNull = true, min = 12)
    @Password(groups = {ExistingUser.class}, allowNull = true, min = 12)
    private String originalPassword;

    private String email;

    private String avatar;

    private boolean enabled;

    private String nickname;

    private Set<String> roles;
    private Set<String> userGroupIds;

    private String securityTokenKey;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public Set<String> getRoles() {
        return roles == null ? Collections.emptySet() : ImmutableSet.copyOf(roles);
    }

    public void setRoles(Set<String> roles) {
        this.roles = roles;
    }

    public String getSecurityTokenKey() {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey) {
        this.securityTokenKey = securityTokenKey;
    }


    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public Set<String> getUserGroupIds() {
        return userGroupIds;
    }

    public void setUserGroupIds(Set<String> userGroupIds) {
        this.userGroupIds = userGroupIds;
    }

    public interface NewUser
            extends Serializable {
        // validation group marker interface for new users.
    }

    public interface ExistingUser
            extends Serializable {
        // validation group marker interface for existing users.
    }

    public interface UpdateAccount
            extends Serializable {
        // validation group marker interface for existing users.
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public String getOriginalPassword() {
        return originalPassword;
    }

    public void setOriginalPassword(String originalPassword) {
        this.originalPassword = originalPassword;
    }
}

