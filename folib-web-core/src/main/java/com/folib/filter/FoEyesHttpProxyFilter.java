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
package com.folib.filter;

import com.folib.components.thirdparty.foeyes.FoEyesProperties;
import jakarta.servlet.*;
import jakarta.servlet.annotation.WebFilter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.*;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StreamUtils;
import org.springframework.web.client.RestTemplate;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

/**
 * @author veadan
 * @date 2024/4/25
 **/
@WebFilter(urlPatterns = "/dependency/*", filterName = "foEyesHttpProxyFilter")
@Slf4j
public class FoEyesHttpProxyFilter implements Filter {

    @Inject
    private FoEyesProperties foEyesProperties;

    @Inject
    private RestTemplate restTemplate;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        log.info("[ {} ] 初始化网关...", this.getClass().getSimpleName());
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest req = (HttpServletRequest) request;
        String requestURI = req.getRequestURI();
        String allowedPath = "/dependency/";
        if (!requestURI.startsWith(allowedPath)) {
            filterChain.doFilter(request, response);
            return;
        }
        log.info("[ {} ] 接收到请求...URI:{}", this.getClass().getSimpleName(), requestURI);
        HttpServletResponse resp = (HttpServletResponse) response;
        // 请求类型
        String method = req.getMethod();
        // 请求方式
        HttpMethod httpMethod = HttpMethod.valueOf(method);
        // 请求头
        MultiValueMap<String, String> headers = parseRequestHeader(req);
        String params = getParamsString(req);
        // 请求体
        byte[] body = parseRequestBody(req);
        requestURI = requestURI.replace(allowedPath, "/api/");
        String url = foEyesProperties.getBaseUrl() + requestURI;
        if (StringUtils.isNotBlank(params)) {
            url = url + "?" + params;
        }
        // 封装发http请求
        RequestEntity requestEntity = new RequestEntity(body, headers, httpMethod, URI.create(url));
        ResponseEntity<String> result = restTemplate.exchange(requestEntity, String.class);
        // 将转发请求得到的结果和响应头返回客户端
        String resultBody = result.getBody();
        HttpHeaders resultHeaders = result.getHeaders();
        MediaType contentType = resultHeaders.getContentType();
        if (contentType != null) {
            resp.setContentType(contentType.toString());
        }
        // 设置响应头部
        resultHeaders.forEach((headerName, headerValues) -> {
            for (String headerValue : headerValues) {
                resp.addHeader(headerName, headerValue);
            }
        });
        // 在getWriter之前执行，否则不生效
        resp.setCharacterEncoding("UTF-8");
        PrintWriter writer = resp.getWriter();
        writer.write(resultBody);
        writer.flush();
    }

    @Override
    public void destroy() {
        log.info("[ {} ] 关闭网关...", this.getClass().getSimpleName());
    }

    /**
     * request header
     *
     * @param request 请求
     * @return 结果
     */
    private MultiValueMap<String, String> parseRequestHeader(HttpServletRequest request) {
        HttpHeaders httpHeaders = new HttpHeaders();
        List<String> headerNames = Collections.list(request.getHeaderNames());
        for (String headerName : headerNames) {
            List<String> headerValues = Collections.list(request.getHeaders(headerName));
            for (String headerValue : headerValues) {
                httpHeaders.add(headerName, headerValue);
            }
        }
        return httpHeaders;
    }

    /**
     * request body
     *
     * @param request 请求
     * @return 结果
     * @throws IOException 异常
     */
    private byte[] parseRequestBody(HttpServletRequest request) throws IOException {
        InputStream inputStream = request.getInputStream();
        return StreamUtils.copyToByteArray(inputStream);
    }

    /**
     * 获取请求参数
     *
     * @param request 请求
     * @return 请求参数
     */
    public static String getParamsString(HttpServletRequest request) {
        StringBuilder params = new StringBuilder();
        Enumeration<String> paramNames = request.getParameterNames();
        while (paramNames.hasMoreElements()) {
            String paramName = (String) paramNames.nextElement();
            String[] values = request.getParameterValues(paramName);
            for (int i = 0; i < values.length; i++) {
                String value = values[i];
                params.append(paramName);
                if (value != null && value.trim().length() > 0) {
                    params.append("=").append(value);
                }
                if (i < values.length - 1 || paramNames.hasMoreElements()) {
                    params.append("&");
                }
            }
        }
        return params.toString();
    }
}