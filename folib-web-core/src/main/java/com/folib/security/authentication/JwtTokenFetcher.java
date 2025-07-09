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

import java.util.Arrays;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public interface JwtTokenFetcher
{

    Logger logger = LoggerFactory.getLogger(JwtTokenFetcher.class);

    String AUTHORIZATION_HEADER = "Authorization";

    String AUTHORIZATION_COOKIE = "token";

    String BEARER_AUTHORIZATION_PREFIX = "Bearer";

    Pattern BEARER_PATTERN = Pattern.compile("Bearer (.*)");

    default Optional<String> getToken(HttpServletRequest request)
    {
        // give priority to header based authentication, because it is more likely to be present
        String tokenHeader = request.getHeader(AUTHORIZATION_HEADER);
        if (StringUtils.isNotBlank(tokenHeader))
        {
            Matcher headerMatcher = BEARER_PATTERN.matcher(tokenHeader);

            if (headerMatcher.matches())
            {
                String token = headerMatcher.group(1);
                logger.debug("Bearer Authorization header found with token {}", token);
                return Optional.of(token);
            }
        }

        // fallback - check if a cookie is present (necessary for EventSource; check gh#1046).
        Optional<Cookie> tokenCookie = Arrays.stream(request.getCookies())
                                             .filter(c -> c.getName()
                                                           .equals(AUTHORIZATION_COOKIE)).findFirst();
        if (tokenCookie.isPresent())
        {
            String token = tokenCookie.get().getValue();
            logger.debug("Bearer Authorization found in cookie with token {}", token);
            return Optional.of(token);
        }

        return Optional.empty();
    }

}
