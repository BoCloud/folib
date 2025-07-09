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
package com.folib.utils;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


public class RequestUtils {


    private static final String STORAGES_PREFIX = "/storages/";
    private static final String BROWSE_PREFIX = "/api/browse/";

    public static HttpServletRequest getCurrentHttpRequest() {

        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if (requestAttributes instanceof ServletRequestAttributes servletRequestAttributes) {
            return servletRequestAttributes.getRequest();
        }
        return null;
    }
    public static String getStorageId() {
        HttpServletRequest request = getCurrentHttpRequest();
        if (request == null) return null;
        return extractPathPart(request, 2);
    }

    public static String getRepositoryId() {
        HttpServletRequest request = getCurrentHttpRequest();
        if (request == null) return null;
        return extractPathPart(request, 3);
    }


    private static String extractPathPart(HttpServletRequest request, int index) {
        //从请求路径中通过正则提取
        String path = request.getRequestURI(); // /storages/public/releases/com/foo/bar
        String contextPath = request.getContextPath();

        if (!contextPath.isEmpty() && path.startsWith(contextPath)) {
            path = path.substring(contextPath.length());
        }
        String[] parts = path.split("/");
        if(request.getServletPath().startsWith(STORAGES_PREFIX)){
            if (parts.length > index && "storages".equals(parts[1])) {
                return parts[index];
            }
        }else if (request.getServletPath().startsWith(BROWSE_PREFIX)){
            if (parts.length > index && "api".equals(parts[1]) && "browse".equals(parts[2])) {
                return parts[index+1];
            }
        }

        return null;
    }

    public static String extractPathFromRequest() {
        HttpServletRequest request = RequestUtils.getCurrentHttpRequest();
        if (request == null) return null;
        // 如 /storages/public/releases/com/example/a.txt
        String uri = request.getRequestURI();
        // 一般是 ""，除非部署到子路径
        String contextPath = request.getContextPath();

        // 去掉 contextPath
        if (!contextPath.isEmpty() && uri.startsWith(contextPath)) {
            uri = uri.substring(contextPath.length());
        }
        // 假设你的路径总是以 /storages/{storageId}/{repositoryId}/ 开头
        // 找第4个斜杠后面的内容
        String[] parts = uri.split("/", 5);

        if(request.getServletPath().startsWith(STORAGES_PREFIX)){
            if (parts.length >= 5) {
                // 第4个斜杠之后的部分
                List<String> paths = new ArrayList<>(parts.length-4);
                for (int i = 4; i < parts.length; i++){
                    paths.add(parts[i]);
                }
                return paths.stream().collect(Collectors.joining("/"));
            }
        }else if (request.getServletPath().startsWith(BROWSE_PREFIX)){
            if (parts.length >= 6) {
                List<String> paths = new ArrayList<>(parts.length-5);
                for (int i = 5; i < parts.length; i++){
                    paths.add(parts[i]);
                }
                return paths.stream().collect(Collectors.joining("/"));
            }
        }
        return null;
    }
}
