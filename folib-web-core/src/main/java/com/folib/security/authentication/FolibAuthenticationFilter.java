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
package com.folib.security.authentication;

import com.folib.configuration.ConfigurationManager;
import com.folib.security.authentication.suppliers.AuthenticationSuppliers;
import com.folib.storage.repository.RepositoryData;
import com.folib.utils.RequestUtils;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.context.SecurityContextRepository;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;


import java.io.IOException;
import java.util.List;
import java.util.Optional;


/**
 * @author veadan
 */
public class FolibAuthenticationFilter
        extends OncePerRequestFilter {

    private static final Logger logger = LoggerFactory.getLogger(FolibAuthenticationFilter.class);

    private final AuthenticationManager authenticationManager;

    private final AuthenticationSuppliers authenticationSuppliers;

    private final AuthenticationEntryPoint authenticationEntryPoint;

    private final ConfigurationManager configurationManager;

    private final Http401AuthenticationEntryPoint customEntryPoint;

    public FolibAuthenticationFilter(AuthenticationSuppliers authenticationSuppliers,
                                     AuthenticationManager authenticationManager,
                                     AuthenticationEntryPoint authenticationEntryPoint,
                                     ConfigurationManager configurationManager  ,Http401AuthenticationEntryPoint customEntryPoint) {
        super();
        this.authenticationSuppliers = authenticationSuppliers;
        this.authenticationManager = authenticationManager;
        this.authenticationEntryPoint=authenticationEntryPoint;
        this.configurationManager = configurationManager;
        this.customEntryPoint =customEntryPoint;
    }
    // 需要跳过的路径列表（与 SecurityConfig 中的路径一致）
    private static final List<String> EXCLUDED_PATHS = List.of(
            "/favicon.ico",
            "/ui/**",
            "/docs/**",
            "/webjars/**",
            "/rest/**",
            "/help/**",
            "/v2/",
            "/"
    );

    private  static final  List<String> ANONYMOUS_URL = List.of("/storages/**","/api/browse/**");

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        // 使用 AntPathMatcher 检查当前请求是否匹配排除路径
        AntPathMatcher matcher = new AntPathMatcher();
        return EXCLUDED_PATHS.stream()
                .anyMatch(pattern -> matcher.match(pattern, request.getServletPath()));
    }
    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
            throws ServletException,
            IOException {
        try {
            // 如果是WebSocket请求，直接跳过认证逻辑
            if (isWebSocketRequest(request)) {
                filterChain.doFilter(request, response);
                return;
            }

            // 仅对非排除路径执行认证逻辑
            if (!shouldNotFilter(request)) {
                // 执行自定义认证逻辑（例如解析 Token、设置 SecurityContext 等）
                Authentication authentication = authenticationSuppliers.supply(request);
                if (authentication == null) {
                    //authentication = SecurityContextHolder.getContext().getAuthentication();
                    logger.debug("Authentication not supplied by any authentication supplier, using [{}] context authentication.",
                            Optional.ofNullable(authentication).map(a -> a.getClass().getSimpleName()).orElse("empty"));
                } else {
                    logger.debug("Supplied [{}] authentication.", authentication.getClass().getSimpleName());
                }

                authentication = provideAuthentication(authentication);
                if (authentication != null) {
                    SecurityContext context = SecurityContextHolder.createEmptyContext();
                    context.setAuthentication(authentication);
                    SecurityContextHolder.setContext(context);
                    // 显式保存认证信息
                    SecurityContextRepository repo = new HttpSessionSecurityContextRepository();
                    repo.saveContext(context, request, response);
                } else {

                    // 如果是匿名访问，则创建一个匿名认证信息
                    if (isAnonymousAuthenticated(request)) {
                        authentication = new AnonymousAuthenticationToken(
                                "anonymousUser", // key
                                "anonymousUser", // principal
                                AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS")
                        );
                        SecurityContext context = SecurityContextHolder.createEmptyContext();
                        context.setAuthentication(authentication);
                        SecurityContextHolder.setContext(context);
                        // 显式保存认证信息
                        SecurityContextRepository repo = new HttpSessionSecurityContextRepository();
                        repo.saveContext(context, request, response);
                    } else {
                        SecurityContextHolder.clearContext();
                        SecurityContextHolder.setStrategyName(SecurityContextHolder.MODE_INHERITABLETHREADLOCAL);
                        //校验token是否有效
                        // Token 无效，返回 401 Unauthorized
                        //response.getWriter().write("Invalid or expired token");
                        //response.setContentType("application/json");
                        //response.setStatus(HttpStatus.UNAUTHORIZED.value());
                        customEntryPoint.commence(request, response, new BadCredentialsException("Invalid or expired Credentials"));
                        return;
                    }
                }
            }
            // 继续执行后续过滤器
            filterChain.doFilter(request, response);
        } catch (AuthenticationException authException) {
            authenticationEntryPoint.commence(request, response, authException);
        }

    }
    private boolean isWebSocketRequest(HttpServletRequest request) {
        return "GET".equalsIgnoreCase(request.getMethod())
                && "websocket".equalsIgnoreCase(request.getHeader("Upgrade"))
                && request.getHeader("Connection") != null
                && request.getHeader("Connection").toLowerCase().contains("upgrade");
    }
    private Authentication provideAuthentication(Authentication authentication) {
        String authenticationName = Optional.ofNullable(authentication)
                .map(a -> a.getClass().getSimpleName())
                .orElse("empty");
        if (authentication == null || authentication.isAuthenticated()) {
            logger.debug("Authentication {} already authenticated or empty, skip providers.", authenticationName);

            return authentication;
        }

        Authentication authResult = authenticationManager.authenticate(authentication);
        logger.debug("Authenticated with {}", authenticationName);

        return authResult;
    }

    private boolean isAnonymousUrl(HttpServletRequest request) {
        AntPathMatcher matcher = new AntPathMatcher();
        return ANONYMOUS_URL.stream().anyMatch(pattern -> matcher.match(pattern, request.getServletPath()));
    }

    //是否匿名访问
    public boolean isAnonymousAuthenticated(HttpServletRequest request) {
        if (!isAnonymousUrl(request)) {
            return false;
        }
        String storageId = RequestUtils.getStorageId();
        String repositoryId = RequestUtils.getRepositoryId();
        if(storageId==null || repositoryId==null){
            return true;
        }else {
            RepositoryData repository = (RepositoryData) configurationManager.getRepository(String.format("%s:%s", storageId, repositoryId));
            if (repository == null || !repository.isAllowAnonymous()) {
                return false;
            } else {
                return repository.isAllowAnonymous();
            }
        }

    }

}
