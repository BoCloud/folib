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

import com.folib.domain.SecurityRole;
import com.folib.domain.User;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * @author veadan
 */
public class EncodedPasswordUser implements User {

    private final User user;
    private final PasswordEncoder passwordEncoder;
    private Set<Long> groupIds = new HashSet<>();
    public EncodedPasswordUser(User user,
                               PasswordEncoder passwordEncoder) {
        this.user = user;
        this.passwordEncoder = passwordEncoder;
    }

    @Override
    public String getUuid() {
        return getUsername();
    }

    @Override
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    public String getEmail() {
        return user.getEmail();
    }

    @Override
    public String getUserType() {
        return user.getUserType();
    }

    @Override
    public String getPassword() {
        String password = user.getPassword();

        return Optional.ofNullable(password)
                .filter(p -> StringUtils.isNoneBlank(p))
                .map(p -> passwordEncoder.encode(p))
                .orElse(password);
    }


    @Override
    public String getOriginalPassword() {
        return user.getOriginalPassword();
    }

    @Override
    public Set<SecurityRole> getRoles() {
        return user.getRoles();
    }

    @Override
    public Set<Long> getGroupIds() {
        return user.getGroupIds();

    }

    @Override
    public Set<String> getUserGroups() {
        return user.getUserGroups();
    }

    @Override
    public Set<String> getUserGroupIds() {
        return user.getUserGroupIds();
    }

    @Override
    public String getSecurityTokenKey() {
        return user.getSecurityTokenKey();
    }

    @Override
    public Boolean isEnabled() {
        return user.isEnabled();
    }

    @Override
    public LocalDateTime getLastUpdated() {
        return user.getLastUpdated();
    }

    @Override
    public String getSourceId() {
        return user.getSourceId();
    }

    @Override
    public String getAvatar() {
        return user.getAvatar();
    }

    @Override
    public String getNickname() {
        return user.getNickname();
    }
}
