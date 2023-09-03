package com.veadan.folib.security.authentication;

import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.services.ConfigurationManagementService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;

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
            response.setHeader("WWW-Authenticate", "Basic realm=\"" + String.format(FOLIB_REALM, configurationManagementService.getMutableConfigurationClone().getInstanceName()) + "\"");
        }

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);

        response.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(message)));
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);      

        response.flushBuffer();
    }
}
