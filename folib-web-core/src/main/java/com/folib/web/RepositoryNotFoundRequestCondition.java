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


import jakarta.servlet.http.HttpServletRequest;

import java.util.Collection;
import java.util.Collections;

import static com.folib.web.Constants.REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE;

/**
 * @author veadan
 */
public class RepositoryNotFoundRequestCondition
        extends ExposableRequestCondition
{

    private final String repositoryId;

    public RepositoryNotFoundRequestCondition(String repositoryId)
    {

        this.repositoryId = repositoryId;
    }

    @Override
    protected void expose(HttpServletRequest request)
    {
        request.setAttribute(REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE, repositoryId);
    }

    @Override
    protected Collection<?> getContent()
    {
        return Collections.singletonList(repositoryId);
    }

    @Override
    protected String getToStringInfix()
    {
        return repositoryId;
    }
}
