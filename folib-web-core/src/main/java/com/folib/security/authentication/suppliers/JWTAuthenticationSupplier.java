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
package com.folib.security.authentication.suppliers;

import com.folib.authentication.api.jwt.JwtAuthentication;
import com.folib.constant.GlobalConstants;
import com.folib.security.authentication.JwtTokenFetcher;
import com.folib.security.exceptions.InvalidTokenException;
import com.folib.users.security.SecurityTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.inject.Inject;
import java.util.Arrays;
import java.util.Optional;

@Component
@Order(1)
public class JWTAuthenticationSupplier
        implements AuthenticationSupplier, JwtTokenFetcher {

    @Lazy
    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request) {
        final Optional<String> optToken = getToken(request);
        if (!optToken.isPresent()) {
            return null;
        }

        final String token = optToken.get();
        String username;
        try {
            username = securityTokenProvider.getSubject(token);
        } catch (InvalidTokenException e) {
            logger.error("token失效", e);
            throw new BadCredentialsException("invalid.token");
        }
        if (GlobalConstants.ANONYMOUS_TOKEN_KEY.equals(username)) {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            Authentication authentication = securityContext.getAuthentication();
            if (authentication instanceof AnonymousAuthenticationToken) {
                return authentication;
            }
        }
        return new JwtAuthentication(username, token);
    }

    @Override
    public boolean supports(HttpServletRequest request) {
        boolean hasHeader = false;
        boolean hasCookie = false;

        // give priority to header based authentication, because it is more likely to be present
        if (request.getHeader(AUTHORIZATION_HEADER) != null) {
            String authHeader = request.getHeader(AUTHORIZATION_HEADER);
            hasHeader = StringUtils.isNotBlank(authHeader) && authHeader.startsWith(BEARER_AUTHORIZATION_PREFIX);
        }
        // fallback - check if a cookie is present (necessary for EventSource; check gh#1046).
        else if (request.getCookies() != null && matchesScope(request)) {
            hasCookie = Arrays.stream(request.getCookies())
                    .anyMatch(c -> c.getName()
                            .equals(AUTHORIZATION_COOKIE));
        }

        if (hasHeader || hasCookie) {
            return true;
        }

        return false;
    }

    private boolean matchesScope(HttpServletRequest request) {
        final String uri = request.getRequestURI();

        // wildcard match
        boolean matches = uri.startsWith("/api") || uri.startsWith("/storages");

        // exclude `/api/ping` since it's under the wild card match, but is called to check for liveliness which
        // means it might contain the `cookie` thus triggering a basic auth (gh#1687).
        if (matches && uri.equals("/api/ping")) {
            matches = false;
        }

        return matches;
    }

}
