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

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;

import com.folib.data.criteria.Expression.ExpOperator;
import com.folib.data.domain.DomainObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link QueryTemplate} implementation for OrientDB engine.
 * 
 * @author veadan
 *
 */
public class OQueryTemplate<R, T extends DomainObject> implements QueryTemplate<R, T>
{
    private static final Logger logger = LoggerFactory.getLogger(OQueryTemplate.class);

    protected EntityManager entityManager;

    public OQueryTemplate()
    {
        super();
    }

    public OQueryTemplate(EntityManager entityManager)
    {
        super();
        this.entityManager = entityManager;
    }

    public EntityManager getEntityManager()
    {
        return entityManager;
    }

    public R select(Selector<T> s)
    {
        String sQuery = calculateQueryString(s);

        Map<String, Object> parameterMap = exposeParameterMap(s.getPredicate());

        logger.info("Executing SQL query:\n" +
                     "\t[{}]\n" +
                     "With parameters:\n" +
                     "\t[{}]",
                     sQuery, parameterMap);
        return null;
    }

    public Map<String, Object> exposeParameterMap(Predicate p)
    {
        return exposeParameterMap(p, 0);
    }

    private Map<String, Object> exposeParameterMap(Predicate p,
                                                   int tokenCount)
    {
        HashMap<String, Object> result = new HashMap<>();
        Expression e = p.getExpression();
        if (e != null && !ExpOperator.IS_NULL.equals(e.getOperator()) && !ExpOperator.IS_NOT_NULL.equals(e.getOperator()))
        {
            result.put(calculateParameterName(e.getProperty(), tokenCount), e.getValue());
        }

        for (Predicate predicate : p.getChildPredicateList())
        {
            result.putAll(exposeParameterMap(predicate, tokenCount++));
        }

        return result;
    }

    public String calculateQueryString(Selector<T> selector)
    {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT ").append(selector.getProjection());
        sb.append(" FROM ").append(selector.getTargetClass().getSimpleName());

        Predicate p = selector.getPredicate();
        if (p.isEmpty())
        {
            return sb.toString();
        }

        sb.append(" WHERE ");
        sb.append(predicateToken(p, 0));

        Paginator paginator = selector.getPaginator();
        if (paginator != null && paginator.getProperty() != null && !paginator.getProperty().trim().isEmpty())
        {
            sb.append(String.format(" ORDER BY %s %s", paginator.getProperty(), paginator.getOrder()));
        }

        if (paginator != null && paginator.getSkip() > 0)
        {
            sb.append(String.format(" SKIP %s", paginator.getSkip()));
        }
        if (paginator != null && paginator.getLimit() > 0)
        {
            sb.append(String.format(" LIMIT %s", paginator.getLimit()));
        }

        if (selector.isFetch())
        {
            sb.append(" FETCHPLAN *:-1");
        }

        return sb.toString();
    }

    protected String predicateToken(Predicate p,
                                    int tokenCount)
    {
        if (p.isEmpty())
        {
            return "";
        }

        StringBuffer sb = new StringBuffer();
        if (p.getExpression() != null)
        {
            sb.append(expressionToken(p.getExpression(), tokenCount));
        }

        for (Predicate predicate : p.getChildPredicateList())
        {
            if (sb.length() > 0)
            {
                sb.append(String.format(" %s ", p.getOperator().name()));
            }
            sb.append(predicateToken(predicate, tokenCount++));
        }

        if (p.isNested())
        {
            sb.insert(0, "(").append(")");
        }

        if (p.isNegated())
        {
            sb.insert(0, " NOT (").append(")");
        }

        return sb.toString();
    }

    protected String expressionToken(Expression e,
                                     int n)
    {
        String experssionLeft = expressionLeftToken(e);
        String operator = expressionOperatorToken(e);
        String expressionRight = expressionRightToken(e, n);

        return new StringBuffer().append(experssionLeft)
                                 .append(operator)
                                 .append(expressionRight)
                                 .toString();
    }

    protected String expressionLeftToken(Expression e)
    {
        switch (e.getOperator())
        {
        case CONTAINS:
            String property = e.getProperty();
            return property.substring(0, property.indexOf("."));
        default:
            break;
        }
        return e.getProperty();
    }

    protected String expressionRightToken(Expression e,
                                          int n)
    {
        switch (e.getOperator())
        {
        case CONTAINS:
            String property = e.getProperty();
            property = property.substring(property.indexOf(".") + 1);

            return String.format("(%s = :%s)", property, calculateParameterName(property, n));
        case IS_NULL:
        case IS_NOT_NULL:
            
            return "";
        default:
            break;
        }
        String property = e.getProperty();
        return String.format(":%s", calculateParameterName(property, n));
    }

    private String calculateParameterName(String property,
                                          int n)
    {
        if (property == null)
        {
            return "";
        }
        property = property.replace(".toLowerCase()", "");
        property = property.replace("@", "");
        return String.format("%s_%s", property.substring(property.lastIndexOf(".") + 1), n);
    }

    protected String expressionOperatorToken(Expression e)
    {
        switch (e.getOperator())
        {
        case EQ:
            return " = ";
        case LE:
            return " <= ";
        case GE:
            return " >=";            
        case LIKE:
            return " LIKE ";
        case CONTAINS:
            return " CONTAINS ";
        case IS_NULL:
            return " IS NULL ";
        case IS_NOT_NULL:
            return " IS NOT NULL ";            
        }
        return null;
    }
}
