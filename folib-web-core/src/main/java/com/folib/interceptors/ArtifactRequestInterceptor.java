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

import static com.folib.web.Constants.REPOSITORY_REQUEST_ATTRIBUTE;

import java.io.IOException;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import com.folib.storage.repository.Repository;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.HandlerMapping;


/**
 * @author veadan
 *
 */
public abstract class ArtifactRequestInterceptor implements HandlerInterceptor
{

    private final String layout;
    
    public ArtifactRequestInterceptor(String layout)
    {
        this.layout = layout;
    }


    public final boolean preHandle(HttpServletRequest request,
                                   HttpServletResponse response,
                                   Object handler)
        throws Exception
    {
        final Map pathVariables = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        if (pathVariables == null)
        {
            return true;
        }

        final Repository repository = (Repository) request.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE);
        if (repository == null || !layout.equals(repository.getLayout()))
        {
            return true;
        }

        final String artifactPath = (String) pathVariables.get("artifactPath");
        if (StringUtils.isBlank(artifactPath))
        {
            return true;
        }

        return preHandle(repository, artifactPath, request, response);
    }

    protected abstract boolean preHandle(Repository repository,
                                         String artifactPath,
                                         HttpServletRequest request,
                                         HttpServletResponse response) throws IOException;

}
