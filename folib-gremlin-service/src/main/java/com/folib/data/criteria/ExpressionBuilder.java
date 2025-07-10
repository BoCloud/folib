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
 * @author veadan
 *
 */
public abstract class ExpressionBuilder<E extends ExpressionBuilder<E, T>, T extends DomainObject>
{
    private Class<T> targetClass;

    private ExpressionDialect dialect;

    private Expression expression = new Expression();

    public ExpressionBuilder(Class<T> targetClass,
                             ExpressionDialect dialect)
    {
        super();
        this.targetClass = targetClass;
        this.dialect = dialect;
    }

    public ExpressionBuilder(Class<T> targetClass)
    {
        this(targetClass, new DefaultExpressionDialect());
    }

    public E of(String attribute)
    {
        expression.setProperty(dialect.parseProperty(attribute));
        return (E) this;
    }

    public E using(String operator)
    {
        expression.setOperator(dialect.parseOperator(operator));
        return (E) this;
    }

    public E with(String value)
    {
        expression.setValue(dialect.parseValue(value));
        return (E) this;
    }

    public Expression build()
    {
        return expression;
    }

}
