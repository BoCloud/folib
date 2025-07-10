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
package com.folib.security.authentication.suppliers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.authentication.api.password.PasswordAuthentication;
import com.folib.controllers.login.LoginController;
import com.folib.controllers.login.LoginInput;
import com.folib.util.RSAUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;

/**
 * Works in conjunction {@link LoginController}
 *
 * @author veadan
 */
@Component
@Order(3)
public class JsonFormLoginSupplier implements AuthenticationSupplier {

    private static final Logger logger = LoggerFactory.getLogger(JsonFormLoginSupplier.class);

    @Inject
    private RSAUtils rsaUtils;

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public Authentication supply(@Nonnull HttpServletRequest request) {
        LoginInput loginInput = null;
        try {
            loginInput = objectMapper.readValue(request.getInputStream(), LoginInput.class);

        } catch (IOException e) {
            throw new BadCredentialsException("invalid.credentials");
        }
        String password = rsaUtils.decrypt(loginInput.getPassword());
        return new PasswordAuthentication(loginInput.getUsername(), password);
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request) {
        return HttpMethod.POST.toString().equalsIgnoreCase(request.getMethod()) &&
                request.getContentType() != null &&
                request.getContentType().contains(MediaType.APPLICATION_JSON_VALUE) &&
                LoginController.REQUEST_MAPPING.equals(request.getRequestURI());
    }
}
