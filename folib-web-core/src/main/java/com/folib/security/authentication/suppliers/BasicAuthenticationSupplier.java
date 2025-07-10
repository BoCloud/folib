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

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import java.io.UnsupportedEncodingException;
import java.util.Base64;

import com.folib.authentication.api.password.PasswordAuthentication;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
@Component
@Order(0)
class BasicAuthenticationSupplier implements AuthenticationSupplier
{

    private static final Logger logger = LoggerFactory.getLogger(BasicAuthenticationSupplier.class);

    private String credentialsCharset = "UTF-8";

    @Override
    public boolean supports(HttpServletRequest request)
    {
        final String header = getAuthenticationHeaderValue(request);
        if (header == null || !header.startsWith("Basic "))
        {
            return false;
        }

        return true;
    }

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        String header = getAuthenticationHeaderValue(request);

        String[] tokens;
        try
        {
            tokens = extractAndDecodeHeader(header);
        }
        catch (UnsupportedEncodingException e)
        {
            throw new BadCredentialsException("Failed to decode basic authentication token", e);
        }

        String username = tokens[0];

        logger.debug("Basic Authentication Authorization header found for user '{}'", username);

        return new PasswordAuthentication(username, tokens[1]);
    }

    private String getAuthenticationHeaderValue(HttpServletRequest request)
    {
        return request.getHeader("Authorization");
    }

    public void setCredentialsCharset(String credentialsCharset)
    {
        Assert.hasText(credentialsCharset, "credentialsCharset cannot be null or empty");
        this.credentialsCharset = credentialsCharset;
    }

    /**
     * Decodes the header into a username and password.
     *
     * @throws BadCredentialsException
     *             if the Basic header is not present or is not valid
     *             Base64
     */
    private String[] extractAndDecodeHeader(String header)
        throws UnsupportedEncodingException
    {
        byte[] base64Token = header.substring(6)
                                   .getBytes(credentialsCharset);
        byte[] decoded;
        try
        {
            decoded = Base64.getDecoder().decode(base64Token);
        }
        catch (IllegalArgumentException e)
        {
            throw new BadCredentialsException("Failed to decode basic authentication token", e);
        }

        String token = new String(decoded, credentialsCharset);

        int delim = token.indexOf(':');

        if (delim == -1)
        {
            throw new BadCredentialsException("Invalid basic authentication token");
        }
        return new String[] { token.substring(0, delim),
                              token.substring(delim + 1) };
    }
}
