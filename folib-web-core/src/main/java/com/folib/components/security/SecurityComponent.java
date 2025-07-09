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
package com.folib.components.security;

import com.folib.constant.GlobalConstants;
import com.folib.security.authentication.JwtTokenFetcher;
import com.folib.users.security.JwtAuthenticationClaimsProvider;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jose4j.jwt.JwtClaims;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Invocation;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class SecurityComponent {

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    private String securityToken;

    public String getSecurityToken() {
        if (StringUtils.isNotBlank(securityToken)) {
            JwtClaims jwtClaims = securityTokenProvider.getClaims(securityToken, false);
            try {
                long expirationTime = jwtClaims.getExpirationTime().getValueInMillis();
                long currentTime = System.currentTimeMillis();
                long interval = 600 * 1000;
                long differTime = expirationTime - currentTime;
                if (differTime <= interval) {
                    log.info("授权token的过期时间还有：{} 毫秒，重新生成", differTime);
                } else {
                    log.info("授权token的过期时间还有：{} 毫秒，继续使用", differTime);
                    return securityToken;
                }
            } catch (Exception ex) {
                log.error("获取授权token的过期时间错误：{}", ExceptionUtils.getStackTrace(ex));
            }
        }
        generateSecurityToken();
        log.info("当前的授权token：{} ", securityToken);
        return securityToken;
    }

    private void generateSecurityToken() {
        String admin = "admin";
        try {
            securityToken = userService.generateSecurityToken(admin);
            log.info("已生成授权token：{}", securityToken);
        } catch (Exception ex) {
            log.error("生成授权token错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    public Invocation.Builder securityTokenHeader(Invocation.Builder builder) {
        builder.header(JwtTokenFetcher.AUTHORIZATION_HEADER, JwtTokenFetcher.BEARER_AUTHORIZATION_PREFIX + " " + getSecurityToken());
        return builder;
    }

    public String generateUserToken() {
        try {
            int expireSeconds = 7200;
            String username = UserUtils.getUsername();
            SpringSecurityUser springSecurityUser = UserUtils.getSpringSecurityUser();
            if (Objects.nonNull(springSecurityUser)) {
                Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
                return securityTokenProvider.getToken(springSecurityUser.getUsername(), claimMap, expireSeconds, null);
            } else if (GlobalConstants.ANONYMOUS_TOKEN_KEY.equals(username)) {
                Map<String, String> claimMap = Collections.singletonMap(GlobalConstants.ANONYMOUS_TOKEN_KEY, username);
                return securityTokenProvider.getToken(username, claimMap, expireSeconds, null);
            }
        } catch (Exception ex) {
            log.info("Generate user token error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }


}