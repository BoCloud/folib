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
package com.folib.config;

import cn.hutool.extra.spring.SpringUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.folib.controllers.support.ErrorResponseEntityBody;
import com.folib.services.ConfigurationManagementService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

/**
 * @author veadan
 * @since 2025-03-12 08:42
 */
public class PathSpecificBasicAuthenticationEntryPoint extends BasicAuthenticationEntryPoint {

    private static final String IS_AJAX_REQUEST_HEADER_NAME = "X-Requested-With";

    private static final String IS_AJAX_REQUEST_HEADER_VALUE = "XMLHttpRequest";

    private static final String FOLIB_REALM = "%s Repository Manager";

    private static final String IS_REQUEST_OPTIONS = "options";

    private static final List<String> DOCKER_USER_AGENT_LIST = Lists.newArrayList("docker", "containerd");

    private static final List<String> USER_AGENT_LIST = Lists.newArrayList("Apache-Maven", "docker", "containerd");

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException {
        String path = request.getRequestURI();
        if (path.startsWith("/dav/")) {
            // 只对 /dav/** 使用自定义逻辑
            ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
            response.addHeader("WWW-Authenticate", String.format("Basic realm=\"%s\"", configurationManagementService.getConfiguration().getInstanceName()));
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Authentication Required");
        } else {
            String message = Optional.ofNullable(authException).map(e -> e.getMessage()).orElse("unauthorized");
            String userAgent = request.getHeader("User-Agent");
            boolean authenticate = !IS_AJAX_REQUEST_HEADER_VALUE.equals(request.getHeader(IS_AJAX_REQUEST_HEADER_NAME)) &&
                    !request.getMethod().equalsIgnoreCase(IS_REQUEST_OPTIONS) && StringUtils.isNotBlank(userAgent) && USER_AGENT_LIST.stream().anyMatch(userAgent::contains);
            if (authenticate) {
                ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
                response.setHeader("WWW-Authenticate", "Basic realm=\"" + String.format(FOLIB_REALM, configurationManagementService.getConfiguration().getInstanceName()) + "\"");
                setDockerTokenUrl(request, response, userAgent);
            }
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(message)));
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.flushBuffer();
        }
    }

    @Override
    public void afterPropertiesSet() {
        ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
        setRealmName(configurationManagementService.getConfiguration().getInstanceName());
        super.afterPropertiesSet();
    }

    private void setDockerTokenUrl(HttpServletRequest request, HttpServletResponse response, String userAgent) {
        if (DOCKER_USER_AGENT_LIST.stream().anyMatch(userAgent::contains)) {
            String originalProtocol = request.getHeader("X-Forwarded-Proto"), https = "https";
            boolean sslEnabled = Boolean.parseBoolean(SpringUtil.getProperty("server.ssl.enabled"));
            if (sslEnabled || https.equals(originalProtocol)) {
                // 使用HTTPS协议
                response.setHeader("WWW-Authenticate", String.format("Bearer realm=\"%stoken\",service=\"%s\"", "https://" + request.getServerName() + "/v2/", request.getServerName()));
            } else {
                // 使用HTTP协议
                response.setHeader("WWW-Authenticate", String.format("Bearer realm=\"%stoken\",service=\"%s\"", "http://" + request.getServerName() + ":" + request.getServerPort() + "/v2/", request.getServerName() + ":" + request.getServerPort()));
            }
        }
    }

}
