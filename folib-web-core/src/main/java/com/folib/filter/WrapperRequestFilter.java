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

import com.google.common.collect.Sets;
import com.folib.wrapper.RequestWrapper;
import jakarta.servlet.*;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;


import java.io.IOException;
import java.util.Collections;
import java.util.Set;

/**
 * @author veadan
 **/
@WebFilter(urlPatterns = "/api/artifact/folib/promotion/*")
public class WrapperRequestFilter implements Filter {

    private static final Set<String> FILTER_PATHS = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/"));

    private static final Set<String> SLICE_PATH = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/slice/upload-web"));

    private static final Set<String> ALLOWED_PATHS = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/upload-files", "/api/artifact/folib/promotion/parseArtifact"
            , "/api/artifact/folib/promotion/slice/upload"
            , "/api/artifact/folib/promotion/header/slice/upload"
    ));

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {

    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) servletRequest;
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        boolean allowedPath = ALLOWED_PATHS.contains(path);
        boolean isMultipart = request.getContentType() != null
                && request.getContentType().toLowerCase().startsWith("multipart/");
        if (allowedPath) {
            filterChain.doFilter(servletRequest, servletResponse);
            return;
        }else if(SLICE_PATH.stream().anyMatch(path::equals)){
            filterChain.doFilter(servletRequest, servletResponse);
            return;
        }
        else if (FILTER_PATHS.stream().anyMatch(path::startsWith) && !isMultipart){
            ServletRequest requestWrapper = new RequestWrapper((HttpServletRequest) servletRequest);
            // 将请求封装并传递下去
            filterChain.doFilter(requestWrapper, servletResponse);
            return;
        }
        filterChain.doFilter(servletRequest, servletResponse);
    }
    @Override
    public void destroy() {

    }
}
