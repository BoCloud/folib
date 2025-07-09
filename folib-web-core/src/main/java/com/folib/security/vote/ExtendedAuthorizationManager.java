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
package com.folib.security.vote;

import com.folib.utils.RequestUtils;
import lombok.extern.slf4j.Slf4j;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.core.Authentication;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ExtendedAuthorizationManager implements AuthorizationManager<MethodInvocation> {


    private final ExtendedAuthoritiesVoter extendedAuthoritiesVoter;

    public ExtendedAuthorizationManager(ExtendedAuthoritiesVoter extendedAuthoritiesVoter) {
        this.extendedAuthoritiesVoter = extendedAuthoritiesVoter;
    }

    @Override
    public AuthorizationDecision check(Supplier<Authentication> authenticationSupplier, MethodInvocation methodInvocation) {
        Authentication authentication = null;
        try {
            authentication = authenticationSupplier.get();
            // 从方法上提取 PreAuthorize 注解中的表达式
            Method method = methodInvocation.getMethod();
            PreAuthorize preAuthorize = method.getAnnotation(PreAuthorize.class);
            List<String> requiredAuthority = null;
            if (preAuthorize != null) {
                String expr = preAuthorize.value();
                ParsedAuthorization parsed = parseAuthorizationExpression(expr);
                if (parsed.type() == AuthorizationType.AUTHENTICATED) {
                    // 检查 authentication.isAuthenticated() && !(instanceof AnonymousToken)
                    return new AuthorizationDecision(authentication.isAuthenticated() && !(authentication instanceof AnonymousAuthenticationToken));
                } else if (parsed.type() == AuthorizationType.AUTHORITY) {
                    // 检查是否有指定权限
                    requiredAuthority = parsed.values();
                }
            }
            // 如果没有显式注解就默认拒绝
            if (requiredAuthority == null) {
                return new AuthorizationDecision(true);
            }
            String storageId = RequestUtils.getStorageId();
            String repositoryId = RequestUtils.getRepositoryId();
            String path = RequestUtils.extractPathFromRequest();
            boolean granted;
            Collection<String> extendedAuthorities = extendedAuthoritiesVoter
                    .getExtendedAuthorities(authentication, storageId, repositoryId, path);
            //判断是否匿名用户
            if (authentication == null || !authentication.isAuthenticated() ||
                    authentication instanceof AnonymousAuthenticationToken) {
                granted = extendedAuthorities != null && requiredAuthority.stream().anyMatch(extendedAuthorities::contains);
                SecurityContextHolder.clearContext();
                SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
                // 设置匿名身份 用于 @PreAuthorize("hasAuthority('')")
                List<String> anonymousAuthorities = extendedAuthorities != null ? extendedAuthorities.stream().toList() : requiredAuthority;
                authentication = new AnonymousAuthenticationToken(
                        "anonymousUser",
                        "anonymousUser",
                        AuthorityUtils.createAuthorityList(anonymousAuthorities)
                );
                log.info("Using anonymous authentication");
                SecurityContextHolder.getContext().setAuthentication(authentication);
            } else {
                granted = extendedAuthorities != null && requiredAuthority.stream().anyMatch(extendedAuthorities::contains);
            }
            return new AuthorizationDecision(granted);
        } catch (RuntimeException e) {
            log.error("Error while checking authorization", e.fillInStackTrace());
            return new AuthorizationDecision(false);
        }
    }


    private record ParsedAuthorization(AuthorizationType type, List<String> values) {
    }

    private ParsedAuthorization parseAuthorizationExpression(String expression) {
        if (expression == null || expression.isBlank()) {
            return new ParsedAuthorization(AuthorizationType.UNKNOWN, List.of());
        }

        if (expression.trim().equals("authenticated")) {
            return new ParsedAuthorization(AuthorizationType.AUTHENTICATED, List.of());
        }

        Pattern pattern = Pattern.compile("has(?:Any)?Authority\\(([^)]+)\\)");
        Matcher matcher = pattern.matcher(expression);
        if (matcher.find()) {
            String group = matcher.group(1);
            String[] parts = group.split(",");
            List<String> authorities = Arrays.stream(parts)
                    .map(s -> s.trim().replaceAll("^['\"]|['\"]$", ""))
                    .toList();
            return new ParsedAuthorization(AuthorizationType.AUTHORITY, authorities);
        }

        return new ParsedAuthorization(AuthorizationType.UNKNOWN, List.of());
    }

    private enum AuthorizationType {
        AUTHORITY, AUTHENTICATED, UNKNOWN
    }

}
