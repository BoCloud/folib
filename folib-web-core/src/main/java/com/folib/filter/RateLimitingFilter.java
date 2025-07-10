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

import com.folib.config.Bucket4jConfig;
import lombok.extern.slf4j.Slf4j;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

@Slf4j
@Component
public class RateLimitingFilter extends OncePerRequestFilter {

    private static final long SLOW_REQUEST_THRESHOLD_MS = 1000;

    @Autowired
    private Bucket4jConfig bucket4jConfig;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
//        long startTime = System.currentTimeMillis();
        if (bucket4jConfig.getBucket().tryConsume(1)) {
            try {
                // 继续执行后续过滤器链
                filterChain.doFilter(request, response);
            } finally {
//                // 计算请求处理时间
//                long duration = System.currentTimeMillis() - startTime;
//                // 如果请求处理时间超过阈值，则认为是慢请求，记录日志
//                if (duration > SLOW_REQUEST_THRESHOLD_MS) {
//                    log.warn("慢请求: {} {} 耗时 {} ms，状态码：{}", request.getMethod(), request.getRequestURI(), duration, response.getStatus());
//                }
            }
        } else {
            log.warn("Rate limit exceeded [{}]", request.getRequestURI());
            response.setContentType("application/json");
            response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
            response.getWriter().write("{ \"error\": \"Rate limit exceeded\" }");
        }
    }
}