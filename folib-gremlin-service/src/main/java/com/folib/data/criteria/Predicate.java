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

import java.util.ArrayList;
import java.util.List;

/**
 * This class is part of Criteria API which is needed to have platform
 * independent query representation.
 * Each {@link Predicate} is search conditions tree node, also have an logical
 * {@link Expression} and nested nodes joined with {@link BooleanOperator}.
 * 
 * @author veadan
 *
 */
public class Predicate
{

    private Expression expression;

    private BooleanOperator operator;

    private List<Predicate> childPredicateList = new ArrayList<>();

    private boolean nested;

    private boolean negated;

    public Predicate()
    {
        super();
    }

    public Expression getExpression()
    {
        return expression;
    }

    public BooleanOperator getOperator()
    {
        return operator;
    }

    public List<Predicate> getChildPredicateList()
    {
        return childPredicateList;
    }

    public Predicate or(Predicate p)
    {
        if (BooleanOperator.AND.equals(this.operator))
        {
            Predicate nestedPredicate = Predicate.empty().or(p);
            add(nestedPredicate);

            return nestedPredicate;
        }

        this.operator = BooleanOperator.OR;
        add(p);

        return this;
    }

    public Predicate and(Predicate p)
    {
        if (BooleanOperator.OR.equals(this.operator))
        {
            Predicate nestedPredicate = Predicate.empty().and(p);
            add(nestedPredicate);

            return nestedPredicate;
        }

        this.operator = BooleanOperator.AND;
        add(p);

        return this;
    }

    private void add(Predicate p)
    {
        if (p == this)
        {
            return;
        }
        this.childPredicateList.add(p);
    }

    public boolean isEmpty()
    {
        return expression == null && childPredicateList.isEmpty();
    }

    public Predicate nested()
    {
        this.nested = true;
        return this;
    }

    public boolean isNested()
    {
        return nested;
    }

    public void setNested(boolean nested)
    {
        this.nested = nested;
    }

    public boolean isNegated()
    {
        return negated;
    }

    public void setNegated(boolean negated)
    {
        this.negated = negated;
    }

    public Predicate negated()
    {
        setNegated(true);
        return this;
    }

    public static Predicate empty()
    {
        return new Predicate();
    }

    public static Predicate of(Expression e)
    {
        Predicate p = new Predicate();
        p.expression = e;
        return p;
    }

    public static enum BooleanOperator
    {
        AND, OR;

        public static BooleanOperator of(String v)
        {
            if ("||".equals(v) || "|".equals(v))
            {
                return BooleanOperator.OR;
            }
            else if ("&&".equals(v) || "&".equals(v))
            {
                return BooleanOperator.AND;
            }
            return BooleanOperator.valueOf(v);
        }
    }

}
