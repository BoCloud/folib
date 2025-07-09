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

import com.fasterxml.jackson.databind.ObjectMapper;

import com.folib.controllers.layout.npm.NpmUser;
import com.folib.authentication.api.password.PasswordAuthentication;
import com.folib.providers.NpmLayoutProvider;

import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.regex.Matcher;

@Component
public class NpmLoginAuthenticationSupplier
        extends LayoutAuthenticationSupplier
{

    @Inject
    private ObjectMapper objectMapper;

    public NpmLoginAuthenticationSupplier()
    {
        super(NpmLayoutProvider.ALIAS);
    }

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        NpmUser npmUser = deserializeNpmUser(request);

        if (!isValidNpmUser(npmUser) ||
                !usernamesMatch(request.getRequestURI(), npmUser.getName()))
        {
            throw new BadCredentialsException("invalid.credentials");
        }

        return new PasswordAuthentication(npmUser.getName(), npmUser.getPassword());
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request)
    {
        if (!super.supports(request))
        {
            return false;
        }

        return RequestMethod.PUT.name().equalsIgnoreCase(request.getMethod()) &&
                request.getRequestURI().contains(NpmLayoutProvider.NPM_USER_PATH);
    }

    private NpmUser deserializeNpmUser(HttpServletRequest request)
    {
        NpmUser npmUser;

        try
        {
            npmUser = objectMapper.readValue(request.getInputStream(), NpmUser.class);
        }
        catch (IOException e)
        {
            npmUser = null;
        }

        return npmUser;
    }

    private boolean isValidNpmUser(NpmUser npmUser)
    {
        return npmUser != null &&
                npmUser.getName() != null &&
                npmUser.getPassword() != null;
    }

    private boolean usernamesMatch(String url, String bodyUsername)
    {
        Matcher urlUsernameMatcher = NpmLayoutProvider.NPM_URL_USERNAME_PATTERN.matcher(url);

        return  urlUsernameMatcher.find() &&
                urlUsernameMatcher.group(1).equals(bodyUsername);
    }

}
