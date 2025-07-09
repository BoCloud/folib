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
package com.folib.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.util.Collections;

/**
 * @author veadan
 * @since 2024-12-31 16:45
 */
@Component
public class SecurityUtils {

    @Autowired
    private UserDetailsService userDetailsService;

    public void setAdminAuthentication() {
        // 创建一个带有管理员权限的 Authentication

        UserDetails admin = userDetailsService.loadUserByUsername("admin");
        UsernamePasswordAuthenticationToken adminAuth = new UsernamePasswordAuthenticationToken(
                admin, // 用户名
                null,    // 密码 (可为 null)
                Collections.singletonList(new SimpleGrantedAuthority("ADMIN")) // 管理员权限
        );

        // 设置到 SecurityContext
        SecurityContextHolder.getContext().setAuthentication(adminAuth);

    }

    public void clearAuthentication() {
        SecurityContextHolder.clearContext();
    }
}
