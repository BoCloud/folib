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
package com.folib.web;

import com.folib.configuration.StoragesConfigurationManager;
import com.folib.interceptors.ArtifactRequestInterceptor;
import com.folib.interceptors.PermissionCheckInterceptor;
import com.folib.interceptors.RepositoryRequestInterceptor;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.BeanFactoryUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.mvc.condition.RequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

/**
 * @author veadan
 * @author veadan
 */
public class CustomReqHandlerMapping
        extends RequestMappingHandlerMapping
{

    @Inject
    private StoragesConfigurationManager configurationManager;
    
    @Override
    protected void detectMappedInterceptors(List<HandlerInterceptor> mappedInterceptors)
    {
        mappedInterceptors.add(new RepositoryRequestInterceptor());
        mappedInterceptors.add(new PermissionCheckInterceptor());
        mappedInterceptors.addAll(BeanFactoryUtils.beansOfTypeIncludingAncestors(obtainApplicationContext(),
                                                                                 ArtifactRequestInterceptor.class, true,
                                                                                 false)
                                                  .values());
    }

    @Override
    protected RequestCondition<ExposableRequestCondition> getCustomTypeCondition(Class<?> handlerType)
    {
        return Optional.ofNullable(AnnotationUtils.findAnnotation(handlerType, LayoutReqMapping.class))
                       .map(m -> new LayoutReqCondition(configurationManager, m.value()))
                       .orElse(null);
    }

    @Override
    protected void handleMatch(RequestMappingInfo info,
                               String lookupPath,
                               HttpServletRequest request)
    {
        if (info.getCustomCondition() instanceof ExposableRequestCondition)
        {
            ((ExposableRequestCondition) info.getCustomCondition()).expose(request);
        }
        super.handleMatch(info, lookupPath, request);
    }

}
