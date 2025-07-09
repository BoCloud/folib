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
package com.folib.aql.grammar;

import java.util.Optional;

import com.folib.data.criteria.Predicate;
import com.folib.domain.ArtifactEntryExpressionBuilder;
import com.folib.aql.grammar.AQLParser.QueryExpContext;
import com.folib.aql.grammar.AQLParser.TokenExpContext;

/**
 * @author veadan
 *
 */
public class AqlQueryVisitor extends AQLBaseVisitor<Predicate>
{

    private Predicate root;

    public AqlQueryVisitor(Predicate predicate)
    {
        this.root = predicate;
    }

    public Predicate getRoot()
    {
        return root;
    }

    @Override
    public Predicate visitQueryExp(QueryExpContext ctx)
    {
        if (ctx.tokenExp() != null)
        {
            Predicate p = visitTokenExp(ctx.tokenExp());

            return negatePredicateIfNeeded(ctx, p);
        }
        else if (ctx.vNestedQueryExp != null)
        {
            Predicate p = visitQueryExp(ctx.vNestedQueryExp).nested();

            return negatePredicateIfNeeded(ctx, p);
        }

        Predicate.BooleanOperator booleanOperator = extractBooleanOperator(ctx);

        Predicate filterExpRoot = Predicate.empty();

        Predicate p1 = visitQueryExp(ctx.vQueryExpLeft);
        Predicate p2 = visitQueryExp(ctx.vQueryExpRight);

        if (Predicate.BooleanOperator.AND.equals(booleanOperator))
        {
            filterExpRoot.and(p1).and(p2);
        }
        else if (Predicate.BooleanOperator.OR.equals(booleanOperator))
        {
            filterExpRoot.or(p1).or(p2);
        }

        return filterExpRoot;
    }

    private Predicate negatePredicateIfNeeded(QueryExpContext ctx,
                                              Predicate p)
    {
        return Optional.ofNullable(ctx.tokenPrefix())
                       .map(c -> c.getText())
                       .filter(c -> c.endsWith("!"))
                       .map(c -> p.negated())
                       .orElse(p);
    }

    @Override
    public Predicate visitTokenExp(TokenExpContext ctx)
    {
        AqlExpressionVisitor nestedVisitor = new AqlExpressionVisitor();
        
        nestedVisitor.visitTokenExp(ctx);
        
        //nestedVisitor.visitTokenKey(ctx.tokenKey());
        //nestedVisitor.visitTokenValue(ctx.tokenValue());

        ArtifactEntryExpressionBuilder expressionBuilder = nestedVisitor.getExpressionBuilder();
        expressionBuilder.using(null);

        return Predicate.of(expressionBuilder.build());
    }

    private Predicate.BooleanOperator extractBooleanOperator(QueryExpContext ctx)
    {
        return Optional.ofNullable(ctx.logicalOp())
                       .map(v -> v.getText())
                       .map(v -> Predicate.BooleanOperator.of(v.toUpperCase()))
                       .orElse(Predicate.BooleanOperator.AND);
    }

}
