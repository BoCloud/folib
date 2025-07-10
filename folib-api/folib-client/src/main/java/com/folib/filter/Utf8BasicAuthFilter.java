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

import org.springframework.util.StringUtils;

import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;

/**
 * @author veadan
 * @date 2025/4/10
 **/
public class Utf8BasicAuthFilter implements ClientRequestFilter {

    private final String username;
    private final String password;

    public Utf8BasicAuthFilter(String username, String password) {
        this.username = username;
        this.password = password;
    }

    @Override
    public void filter(ClientRequestContext requestContext) throws IOException {
        try {
            if (!StringUtils.hasText(username) || !StringUtils.hasText(password)) {
                return;
            }
            // 生成 UTF-8 编码的 Basic 认证头
            String credentials = username + ":" + password;
            String encodedAuth = Base64.getEncoder()
                    .encodeToString(credentials.getBytes(StandardCharsets.UTF_8));
            String authHeader = "Basic " + encodedAuth;

            // 添加或替换 Authorization 头
            MultivaluedMap<String, Object> headers = requestContext.getHeaders();
            if (Objects.isNull(headers)) {
                headers = new MultivaluedHashMap();
            }
            if (!headers.containsKey(HttpHeaders.AUTHORIZATION)) {
                headers.putSingle(HttpHeaders.AUTHORIZATION, authHeader);
            }
            if (!headers.containsKey(HttpHeaders.USER_AGENT)) {
                headers.putSingle(HttpHeaders.USER_AGENT, "FoLibrary/1.0.0");
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
