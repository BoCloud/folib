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

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.folib.controllers.support.ErrorResponseEntityBody;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.services.ConfigurationManagementService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

@Component
public class Http401AuthenticationEntryPoint implements AuthenticationEntryPoint
{

    private static final String IS_AJAX_REQUEST_HEADER_NAME = "X-Requested-With";

    private static final String IS_AJAX_REQUEST_HEADER_VALUE = "XMLHttpRequest";

    private static final String FOLIB_REALM = "%s Repository Manager";

    private static final String IS_REQUEST_OPTIONS = "options";

    private static final List<String> USER_AGENT_LIST = Lists.newArrayList("Apache-Maven");

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public void commence(HttpServletRequest request,
                         HttpServletResponse response,
                         AuthenticationException authException)
            throws IOException
    {
        String message = Optional.ofNullable(authException).map(e -> e.getMessage()).orElse("unauthorized");

        String userAgent = request.getHeader("User-Agent");
        boolean authenticate = !IS_AJAX_REQUEST_HEADER_VALUE.equals(request.getHeader(IS_AJAX_REQUEST_HEADER_NAME)) &&
                !request.getMethod().equalsIgnoreCase(IS_REQUEST_OPTIONS) && StringUtils.isNotBlank(userAgent) && USER_AGENT_LIST.stream().anyMatch(userAgent::contains);
        if (authenticate)
        {
            ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
            response.setHeader("WWW-Authenticate", "Basic realm=\"" + String.format(FOLIB_REALM, configurationManagementService.getConfiguration().getInstanceName()) + "\"");
        }

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);

        response.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(message)));
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);

        response.flushBuffer();
    }
}
