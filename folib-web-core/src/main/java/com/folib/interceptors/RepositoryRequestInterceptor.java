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
package com.folib.interceptors;

import com.folib.storage.repository.Repository;


import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.servlet.HandlerInterceptor;

import java.io.IOException;

import static com.folib.web.Constants.*;


public class RepositoryRequestInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle( HttpServletRequest request,
                              HttpServletResponse response,
                              Object handler) throws IOException {
        final String storageId = (String) request.getAttribute(STORAGE_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (storageId != null) {
            response.sendError(HttpStatus.NOT_FOUND.value(), "The specified storage does not exist!");
            return false;
        }

        final String repositoryId = (String) request.getAttribute(REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (repositoryId != null) {
            response.sendError(HttpStatus.NOT_FOUND.value(), "The specified repository does not exist!");
            return false;
        }

        final Repository repository = (Repository) request.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE);
        if (repository != null && !repository.isInService()) {
            response.sendError(HttpStatus.SERVICE_UNAVAILABLE.value(), "Repository is not in service...");
            return false;
        }

        return true;
    }
}
