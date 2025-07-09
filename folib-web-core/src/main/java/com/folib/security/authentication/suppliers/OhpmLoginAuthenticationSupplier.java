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
import com.folib.enums.NpmSubLayout;
import com.folib.providers.NpmLayoutProvider;
import com.folib.security.exceptions.InvalidTokenException;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.util.CacheUtil;
import com.folib.utils.UrlUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Objects;

@Component
@Slf4j
public class OhpmLoginAuthenticationSupplier
        extends LayoutAuthenticationSupplier {

    public OhpmLoginAuthenticationSupplier() {
        super(NpmLayoutProvider.ALIAS);
    }

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    String AUTHORIZATION_HEADER = "Authorization";

    String USER_AGENT = "user-agent";

    String OHPM_USER_AGENT = "ohpm";

    String BEARER_AUTHORIZATION_PREFIX = "Bearer";

    String BASIC_AUTHORIZATION_PREFIX = "Basic";

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request) {
        String token = request.getHeader(AUTHORIZATION_HEADER);
        String storageId = UrlUtils.getCurrentStorageId();
        String repositoryId = UrlUtils.getCurrentRepositoryId();
        boolean hasHeader = StringUtils.isNotBlank(token) && !token.startsWith(BEARER_AUTHORIZATION_PREFIX) && !token.startsWith(BASIC_AUTHORIZATION_PREFIX) && isLayoutRepository(storageId, repositoryId);
        if (!hasHeader) {
            throw new BadCredentialsException("invalid.credentials");
        }
        String username;
        try {
            username = securityTokenProvider.getSubject(token);
        } catch (InvalidTokenException e) {
            log.error("OHPM token失效", e);
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
    public boolean supports(@Nonnull HttpServletRequest request) {
        if (!super.supports(request)) {
            return false;
        }
        boolean hasHeader = false;
        if (StringUtils.isNotBlank(request.getHeader(AUTHORIZATION_HEADER)) && StringUtils.isNotBlank(request.getHeader(USER_AGENT))) {
            String authHeader = request.getHeader(AUTHORIZATION_HEADER);
            String storageId = UrlUtils.getCurrentStorageId();
            String repositoryId = UrlUtils.getCurrentRepositoryId();
            hasHeader = StringUtils.isNotBlank(authHeader) && !authHeader.startsWith(BEARER_AUTHORIZATION_PREFIX) && !authHeader.startsWith(BASIC_AUTHORIZATION_PREFIX) && isLayoutRepository(storageId, repositoryId);
        }
        if (hasHeader) {
            return true;
        }
        return false;
    }

    private boolean isLayoutRepository(String storageId, String repositoryId) {
        if (StringUtils.isBlank(storageId) || StringUtils.isBlank(repositoryId)) {
            return false;
        }
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (Objects.isNull(storage)) {
                return false;
            }
            repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                return false;
            }
            cacheUtil.put(key, repository);
        }
        return NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout());
    }
}
