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
package com.folib.data.criteria;

import com.folib.data.domain.DomainObject;

/**
 * This class represent a final and ready to perform Query with target
 * projection and search expressions.
 * 
 * @author veadan
 *
 */
public class Selector<T extends DomainObject>
{

    private Class<T> targetClass;

    // TODO: we need something like ProjectionExpression here instead of just
    // String
    private String projection = "*";

    private Predicate predicate;

    private boolean fetch;
    
    private Paginator paginator = new Paginator();

    public Selector(Class<T> targetClass)
    {
        super();
        this.targetClass = targetClass;
    }

    public Class<T> getTargetClass()
    {
        return targetClass;
    }

    public Predicate getPredicate()
    {
        return predicate;
    }

    public String getProjection()
    {
        return projection;
    }

    public Selector<T> select(String projection)
    {
        this.projection = projection;
        return this;
    }

    public Predicate where(Expression e)
    {
        return this.predicate = Predicate.of(e);
    }

    public Predicate where(Predicate p)
    {
        return this.predicate = p;
    }

    public boolean isFetch()
    {
        return fetch;
    }

    public Selector<T> fetch()
    {
        this.fetch = true;
        return this;
    }

    public Paginator getPaginator()
    {
        return paginator;
    }

    public Selector<T> with(Paginator paginator)
    {
        this.paginator = paginator;
        return this;
    }

}
