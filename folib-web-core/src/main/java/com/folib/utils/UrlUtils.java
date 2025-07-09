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

import com.folib.scanner.common.exception.BusinessException;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Optional;

/**
 * @author
 */
@Slf4j
public class UrlUtils {

    private UrlUtils() {
    }

    public static String getRequestUri() {
        HttpServletRequest servletRequest = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        return servletRequest.getRequestURI();
    }

    public static HttpServletRequest getRequest() {
        try {
            return ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        } catch (Exception ignore) {

        }
        return null;
    }

    public static String getCurrentStorageId() {
        return getSubPath(getRequestUri(), 2);
    }

    public static String getCurrentRepositoryId() {
        return getSubPath(getRequestUri(), 3);
    }

    private static String getSubPath(String url,
                                     int index) {
        String[] args = url.split("/");
        if (args.length < index + 1) {
            return null;
        }
        return args[index];
    }


    public static String[] parsePath(String artifactPath) {
        try {
            URL url = new URL(artifactPath);
            String path = url.getPath();
            String hostUrl = url.getHost();
            String[] parts = path.split("/");
            String storageId = parts[1];
            String repositoryId = parts[2];
            return new String[]{storageId, repositoryId, hostUrl};
        } catch (Exception e) {
            // URL 格式不正确或解析失败
            throw new BusinessException(String.format("%s URL 格式不正确或解析失败", artifactPath));
        }
    }

    public static Integer getPort(String urlStr) {
        if (urlStr.startsWith("https")) {
            return 443;
        }
        try {
            final URL url = new URL(urlStr);
            return Optional.of(url.getPort()).map(p -> p < 0 ? 80 : p).get();
        } catch (MalformedURLException e) {
            log.error("解析端口错误", e);
            return null;
        }
    }

    public static String getHost(String urlStr) {
        try {
            final URL url = new URL(urlStr);
            return url.getHost();
        } catch (MalformedURLException e) {
            log.error("解析Host错误", e);
            return null;
        }
    }

    public static String addQuery(String urlStr, String key, String value) {
        final StringBuilder builder = new StringBuilder(urlStr);
        if (!urlStr.contains("?")) {
            builder.append("?");
        }
        if (!builder.toString().endsWith("?")) {
            builder.append("&");
        }

        builder.append(key).append("=").append(value);
        return builder.toString();
    }

}
